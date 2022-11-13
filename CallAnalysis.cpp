#include "CallAnalysis.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Module.h"
#include <cstddef>
#include <deque>
#include <functional>
#include <string>
#include <unordered_set>
#include <utility>

// helper type for the visitor #4
template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
// explicit deduction guide (not needed as of C++20)
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

namespace call_analysis {

size_t Function::Hasher::operator()(const Function &c) const {
  return std::hash<llvm::Function *>()(c.fn);
}

size_t FuncParam::Hasher::operator()(const FuncParam &c) const {
  return std::hash<llvm::Argument *>()(c.arg) + c.idx;
}

size_t CallInst::Hasher::operator()(const CallInst &c) const {
  size_t hash = c.lineLoc;
  for (auto &operand : c.operands) {
    hash += CallAnalysisValue::Hasher()(operand);
  }
  return hash;
}

struct HashVisitor {
  size_t hash;
  template <class T> void operator()(T &t) {
    typename T::Hasher hasher;
    hash = hasher(t);
  }
};

size_t CallAnalysisValue::Hasher::operator()(const CallAnalysisValue &c) const {
  HashVisitor visitor;
  std::visit(visitor, c.cv);
  return visitor.hash;
}

void Function::Evaluate(CallAnalysis &callAnalysis,
                        const std::deque<Function> &fnArgs,
                        unsigned callerLineLoc, Set &results) {
#ifdef ANALYSIS_INTERNAL
  llvm::errs() << ToString();
  llvm::errs() << "(";
  for (unsigned i = 0; i < fnArgs.size(); ++i) {
    llvm::errs() << fnArgs[i].ToString()
                 << (i == fnArgs.size() - 1 ? "" : ", ");
  }
  llvm::errs() << ")\n";
#endif

  CallAnalysis::FunctionWithArgs fnWithArgs(*this, callerLineLoc, fnArgs);
  if (auto it = callAnalysis.cachedFnWithArgsResults.find(fnWithArgs);
      it != callAnalysis.cachedFnWithArgsResults.end()) {
    results.merge(it->second);
    return;
  }

  Function::Set fnRetResults;
  callAnalysis.cachedFnWithArgsResults[fnWithArgs] =
      fnRetResults; // placeholder to avoid recursive infinitely

  std::unordered_map<CallInst, Function::Set, CallInst::Hasher>
      cachedCallInstResults;
  if (auto it = callAnalysis.fnCallCtxMap.find(*this);
      it != callAnalysis.fnCallCtxMap.end()) {
    for (auto &callee : it->second.callees) {
      Function::Set callInstResults;
      std::vector<Function> callerArgs;
      for (auto &arg : fnArgs) {
        callerArgs.push_back(arg);
      }
      callee.Evaluate(callAnalysis, callerArgs, callInstResults);
      cachedCallInstResults.insert({callee, callInstResults});
    }

    for (auto &ret : it->second.returnValues) {
      std::visit(overloaded{[&](FuncParam &param) {
                              fnRetResults.insert(fnArgs[param.idx]);
                            },
                            [&](Function &f) { fnRetResults.insert(f); },
                            [&](CallInst &c) {
                              auto it = cachedCallInstResults.find(c);
                              assert(it != cachedCallInstResults.end());
                              fnRetResults.merge(it->second);
                            }},
                 ret.cv);
    }
    callAnalysis.cachedFnWithArgsResults[fnWithArgs] = fnRetResults;
    results.merge(fnRetResults);
  } else {
    llvm::errs() << "ERROR: Fn not found in [fnCallCtxMap]!\n";
    exit(-1);
  }
}

class EvalCalleeArgsVisitor {
private:
  CallAnalysis &callAnalysis;
  const std::vector<Function> &callerArgs;
  std::vector<Function::Set> callOperandSets;

public:
  using CallOperands = std::deque<Function>;
  using CallOperandsList = std::vector<CallOperands>;

  explicit EvalCalleeArgsVisitor(CallAnalysis &callAnalysis,
                                 const std::vector<Function> &callerArgs)
      : callerArgs(callerArgs), callAnalysis(callAnalysis) {}

  void operator()(Function &f) { callOperandSets.push_back(Function::Set{f}); }
  void operator()(FuncParam &fa) {
    callOperandSets.push_back(Function::Set{callerArgs[fa.idx]});
  }
  void operator()(CallInst &c) {
#ifdef ANALYSIS_INTERNAL
    llvm::errs() << "(Call as Arg) ";
#endif
    callOperandSets.emplace_back();
    c.Evaluate(callAnalysis, callerArgs, callOperandSets.back());
  }

  CallOperandsList CalculateCallOperands() {
    CallOperandsList result;
    CallOperands curCallOperands;
    doCalculateCallOperands(result, curCallOperands, 0);
    assert(curCallOperands.empty());
    return result;
  }

private:
  void doCalculateCallOperands(CallOperandsList &result,
                               CallOperands &curCallOperands, unsigned level) {
    if (level == callOperandSets.size()) {
      result.push_back(curCallOperands);
      return;
    }
    for (const Function &operand : callOperandSets[level]) {
      curCallOperands.push_back(operand);
      doCalculateCallOperands(result, curCallOperands, level + 1);
      curCallOperands.pop_back();
    }
  }
};

void CallInst::Evaluate(CallAnalysis &callAnalysis,
                        const std::vector<Function> &callerArgs,
                        Function::Set &results) {
  EvalCalleeArgsVisitor visitor(callAnalysis, callerArgs);
  for (auto &operand : operands) {
    std::visit(visitor, operand.cv);
  }
  auto callOperandsList = visitor.CalculateCallOperands();
  for (const auto &callOperands : callOperandsList) {
    assert(callOperands.size() == this->operands.size());

    auto fnArgs = callOperands;
    Function callee = fnArgs.front();
    fnArgs.pop_front();

#ifdef ANALYSIS_INTERNAL
    llvm::errs() << "line:" << this->lineLoc;
    ++callAnalysis.stackFrameId;
    for (unsigned i = 0; i < callAnalysis.stackFrameId; ++i) {
      llvm::errs() << "  ";
    }
#endif

    if (auto it = callAnalysis.fnCallCtxMap.find(callee);
        it != callAnalysis.fnCallCtxMap.end()) {
#ifdef ANALYSIS_INTERNAL
      llvm::errs() << "> ";
#endif
      Function::Set calleeResult;
      callee.Evaluate(callAnalysis, fnArgs, this->lineLoc, calleeResult);
      results.merge(calleeResult);
    } else {
#ifdef ANALYSIS_INTERNAL
      llvm::errs() << "$ " << callee.ToString() << "\n";
#endif
    }
    callAnalysis.actuallyCallSet.insert(std::make_pair(lineLoc, callee));
#ifdef ANALYSIS_INTERNAL
    --callAnalysis.stackFrameId;
#endif
  }
}

std::string CallInst::ToString() const {
  std::string result = "[Call line:";
  result += std::to_string(lineLoc);
  for (auto &op : operands) {
    result += ' ';
    result += op.ToString();
  }
  result += ']';
  return result;
}

void CallInst::combineCallInst(
    unsigned lineLoc,
    const std::vector<std::vector<CallAnalysisValue>> &operandsVec,
    unsigned level, std::vector<CallAnalysisValue> &operands,
    std::vector<CallInst> &output) {
  if (level == operandsVec.size()) {
    output.emplace_back(lineLoc, operands);
    return;
  }
  for (auto &v : operandsVec[level]) {
    operands.push_back(v);
    combineCallInst(lineLoc, operandsVec, level + 1, operands, output);
    operands.pop_back();
  }
}

static bool isFnOrFnPtrTy(llvm::Type *ty) {
  if (ty->isFunctionTy())
    return true;
  if (auto *pt = llvm::dyn_cast<llvm::PointerType>(ty)) {
    return isFnOrFnPtrTy(pt->getElementType());
  }
  return false;
}

bool CallInst::operator==(const CallInst &other) const {
  if (operands.size() != other.operands.size() || lineLoc != other.lineLoc) {
    return false;
  }
  for (unsigned i = 0; i < operands.size(); ++i) {
    if (!(operands[i] == other.operands[i])) {
      return false;
    }
  }
  return true;
}

std::vector<CallInst> CallInst::SplitPhi(llvm::CallInst *c,
                                         std::vector<FuncParam> &args) {
  std::vector<std::vector<CallAnalysisValue>> splittedOperands;
  splittedOperands.push_back(
      CallAnalysisValue::SplitPhi(c->getCalledOperand(), args));
  for (unsigned i = 0; i < c->arg_size(); ++i) {
    llvm::Value *op = c->getOperand(i);
    if (auto ty = op->getType(); isFnOrFnPtrTy(ty)) {
      splittedOperands.push_back(CallAnalysisValue::SplitPhi(op, args));
    }
  }

  std::vector<CallInst> result;
  std::vector<CallAnalysisValue> ops;
  combineCallInst(c->getDebugLoc().getLine(), splittedOperands, 0, ops, result);
  assert(ops.empty());
  return result;
}

struct ToStringVisitor {
  std::string result;
  template <class T> void operator()(T &t) { result = t.ToString(); }
};

std::string CallAnalysisValue::ToString() const {
  ToStringVisitor visitor;
  std::visit(visitor, cv);
  return visitor.result;
}

void CallAnalysis::Run(llvm::Module &M) {
  Clear();
  for (auto it = M.begin(); it != M.end(); it++) {
    llvm::Function &f = *it;
    Function fn{.fn = &f};
    {
      unsigned argIdx = 0;
      for (auto &arg : f.args()) {
        if (isFnOrFnPtrTy(arg.getType())) {
          fnCallCtxMap[fn].funcParams.push_back(FuncParam{argIdx, &arg});
          ++argIdx;
        }
      }
    }

    for (llvm::BasicBlock &block : f.getBasicBlockList()) {
      for (auto &inst : block.instructionsWithoutDebug()) {
        if (llvm::ReturnInst *retInst =
                llvm::dyn_cast<llvm::ReturnInst>(&inst)) {
          if (auto ty = f.getReturnType();
              ty->isPointerTy() || ty->isPointerTy()) {
            auto *v = retInst->getReturnValue();
            auto splittedCav =
                CallAnalysisValue::SplitPhi(v, fnCallCtxMap[fn].funcParams);
            auto &retValues = fnCallCtxMap[fn].returnValues;
            assert(retValues.empty());
            retValues = std::move(splittedCav);
          }
        } else if (llvm::CallInst *callInst =
                       llvm::dyn_cast<llvm::CallInst>(&inst)) {
          auto splittedCallInst =
              CallInst::SplitPhi(callInst, fnCallCtxMap[fn].funcParams);
          for (auto &c : splittedCallInst) {
            fnCallCtxMap[fn].callees.push_back(c);
          }
        }
      }
    }
  }

#ifdef ANALYSIS_INTERNAL
  PrintCallCtxMap();

  llvm::errs() << "\n[Evaluate FnCall]\n";
#endif

  // evaluate callInsts and functions
  for (auto &[fn, fnNode] : fnCallCtxMap) {
    if (fnNode.funcParams.empty()) { // root function
#ifdef ANALYSIS_INTERNAL
      llvm::errs() << "Enter Root Function: " << fn.ToString() << '\n';
#endif
      for (auto &callInst : fnNode.callees) {
        std::vector<Function> emptyCallerArgs;
        Function::Set resultSet;
        callInst.Evaluate(*this, emptyCallerArgs, resultSet);
      }
    }
  }

#ifdef ANALYSIS_INTERNAL
  llvm::errs() << "\n[actuallyCallSet]\n";
#endif
  unsigned lastLine = 0;
  for (auto [line, f] : actuallyCallSet) {
    assert(line > 0);
    if (line == lastLine) {
      llvm::errs() << ", " << f.fn->getName();
    } else {
      llvm::errs() << (lastLine == 0 ? "" : "\n");
      llvm::errs() << line << " : " << f.fn->getName();
    }
    lastLine = line;
  }
  llvm::errs() << '\n';
}

void CallAnalysis::PrintCallCtxMap() const {
  llvm::errs() << "[fnCallCtxMap]\n";
  for (auto &[fn, fnNode] : fnCallCtxMap) {
    llvm::errs() << fn.fn->getName();
    llvm::errs() << "(";
    for (unsigned i = 0; i < fnNode.funcParams.size(); ++i) {
      llvm::errs() << fnNode.funcParams[i].ToString();
      llvm::errs() << (i == fnNode.funcParams.size() - 1 ? "" : ", ");
    }
    llvm::errs() << "):\n  callees:\n";
    for (auto &v : fnNode.callees) {
      llvm::errs() << "    " << v.ToString() << "\n";
    }
    llvm::errs() << "  returns:\n";
    for (auto &v : fnNode.returnValues) {
      llvm::errs() << "    " << v.ToString() << "\n";
    }
  }
}

} // namespace call_analysis