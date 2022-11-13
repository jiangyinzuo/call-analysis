#include "CallAnalysis.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Module.h"
#include <cstddef>
#include <functional>

namespace call_analysis {

size_t Function::Hasher::operator()(const Function &c) const {
  return std::hash<llvm::Function *>()(c.fn);
}

size_t FuncArgument::Hasher::operator()(const FuncArgument &c) const {
  return std::hash<llvm::Argument *>()(c.arg) + c.idx;
}

size_t CallInst::Hasher::operator()(const CallInst &c) const {
  size_t hash = 0;
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
  visitor(c);
  return visitor.hash;
}

struct EvaluateVisitor {
  const std::vector<Function> &boundParams;
  std::vector<Function> &results;
  std::vector<Function> arguments;

  explicit EvaluateVisitor(const std::vector<Function> &args,
                           std::vector<Function> &results)
      : boundParams(args), results(results) {}

  void operator()(Function &f) { arguments.push_back(f); }
  void operator()(FuncArgument &fa) {
    arguments.push_back(boundParams[fa.idx]);
  }
  void operator()(CallInst &c) {
    // c.TryEvaluate(const std::vector<Function> &args, std::vector<Function> &results) TODO:
  }
};

bool CallInst::TryEvaluate(const std::vector<Function> &args,
                           std::vector<Function> &results) {
  EvaluateVisitor visitor(args, results);
  for (auto &operand : operands) {
    std::visit(visitor, operand.cv);
  }
  return false;
}

std::string CallInst::ToString() const {
  std::string result = "[Call";
  for (auto &op : operands) {
    result += ' ';
    result += op.ToString();
  }
  result += ']';
  return result;
}

void CallInst::combineCallInst(
    const std::vector<std::vector<CallAnalysisValue>> &operandsVec,
    unsigned level, std::vector<CallAnalysisValue> &operands,
    std::vector<CallInst> &output) {
  if (level == operandsVec.size()) {
    output.emplace_back(operands);
    return;
  }
  for (auto &v : operandsVec[level]) {
    operands.push_back(v);
    combineCallInst(operandsVec, level + 1, operands, output);
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
  if (operands.size() != other.operands.size()) {
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
                                         std::vector<FuncArgument> &args) {
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
  combineCallInst(splittedOperands, 0, ops, result);
  assert(ops.empty());
  return result;
}

std::optional<Function> CallAnalysisValue::TryEvaluate() {}

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
    {
      unsigned argIdx = 0;
      for (auto &arg : f.args()) {
        if (isFnOrFnPtrTy(arg.getType())) {
          lazyEvaledFn[&f].args.push_back(FuncArgument{argIdx, &arg});
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
                CallAnalysisValue::SplitPhi(v, lazyEvaledFn[&f].args);
            auto &retValues = lazyEvaledFn[&f].returnValues;
            assert(retValues.empty());
            retValues = std::move(splittedCav);
          }
        } else if (llvm::CallInst *callInst =
                       llvm::dyn_cast<llvm::CallInst>(&inst)) {
          auto splittedCallInst =
              CallInst::SplitPhi(callInst, lazyEvaledFn[&f].args);
          for (auto &c : splittedCallInst) {
            lazyEvaledFn[&f].callees.push_back(c);
          }
          lazyEvaledCallInst[callInst] = std::move(splittedCallInst);
        }
      }
    }
  }

#ifdef ANALYSIS_INTERNAL
  PrintMap();
#endif

  // evaluate callInsts and functions
  for (auto &[llvmFn, fnNode] : lazyEvaledFn) {
    if (fnNode.args.empty()) { // root function
      for (auto &callee : fnNode.callees) {
        assert(evaluatedCallResult[callee].empty());
      }
    }
  }
}

void CallAnalysis::PrintMap() const {
  llvm::errs() << "[lazyEvaledFn]\n";
  for (auto &[fn, fnNode] : lazyEvaledFn) {
    llvm::errs() << fn->getName();
    llvm::errs() << "(";
    for (unsigned i = 0; i < fnNode.args.size(); ++i) {
      llvm::errs() << fnNode.args[i].ToString();
      llvm::errs() << (i == fnNode.args.size() - 1 ? "" : ", ");
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
  llvm::errs() << "\n[lazyEvaledCallInst]\n";
  for (auto &[c, values] : lazyEvaledCallInst) {
    llvm::errs() << c->getDebugLoc().getLine() << " : ";
    for (unsigned i = 0; i < values.size(); ++i) {
      llvm::errs() << values[i].ToString()
                   << (i == values.size() - 1 ? "\n" : "\n     ");
    }
  }
}

} // namespace call_analysis