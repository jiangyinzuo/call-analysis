#pragma once
#include <cstddef>
#include <llvm/IR/LLVMContext.h>

#include "llvm/IR/Instructions.h"
#include <llvm/IR/Function.h>
#include <llvm/Support/raw_ostream.h>

#include <deque>
#include <optional>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <variant>

namespace call_analysis {
struct Function;
struct FuncParam;
struct CallInst;
struct CallAnalysisValue;
class CallAnalysis;

struct Function {
  struct Hasher {
    size_t operator()(const Function &c) const;
  };

  struct Context {
    std::vector<FuncParam> funcParams;
    std::vector<CallInst> callees;
    std::vector<CallAnalysisValue> returnValues;
  };

  using CallCtxMap = std::unordered_map<Function, Context, Hasher>;
  using Set = std::unordered_set<Function, Function::Hasher>;

  llvm::Function *fn;

  bool operator==(const Function &other) const { return fn == other.fn; }
  bool operator<(const Function &other) const {
    return reinterpret_cast<size_t>(fn) < reinterpret_cast<size_t>(other.fn);
  }

  void Evaluate(CallAnalysis &callAnalysis, const std::deque<Function> &fnArgs,
                unsigned callerLineLoc, Set &results);

  std::string ToString() const {
    std::string result = "[Fn ";
    result += fn->getName();
    result += ']';
    return result;
  }
};

struct FuncParam {
  struct Hasher {
    size_t operator()(const FuncParam &c) const;
  };

  unsigned idx;
  llvm::Argument *arg;

  explicit FuncParam(unsigned idx, llvm::Argument *arg) : idx(idx), arg(arg) {}

  bool operator==(const FuncParam &other) const {
    assert((idx == other.idx) == (arg == other.arg));
    return idx == other.idx && arg == other.arg;
  }

  std::string ToString() const {
    std::string result = "[Arg ";
    result += std::to_string(idx);
    result += ' ';
    result += arg->getName();
    result += ']';
    return result;
  }
};

struct CallInst {
  struct Hasher {
    size_t operator()(const CallInst &c) const;
  };

  unsigned lineLoc;
  std::vector<CallAnalysisValue> operands;

  CallInst() = default;
  explicit CallInst(unsigned lineLoc,
                    const std::vector<CallAnalysisValue> &operands)
      : lineLoc(lineLoc), operands(operands) {}

  bool operator==(const CallInst &other) const;

  static std::vector<CallInst> SplitPhi(llvm::CallInst *c,
                                        std::vector<FuncParam> &args);

  void Evaluate(CallAnalysis &callAnalysis,
                const std::vector<Function> &callerArgs,
                Function::Set &results);

  std::string ToString() const;

private:
  friend class CallAnalysisValue;
  static void combineCallInst(
      unsigned lineLoc,
      const std::vector<std::vector<CallAnalysisValue>> &operandsVec,
      unsigned level, std::vector<CallAnalysisValue> &operands,
      std::vector<CallInst> &output);
};

struct CallAnalysisValue {
  struct Hasher {
    size_t operator()(const CallAnalysisValue &c) const;
  };

  std::variant<CallInst, FuncParam, Function> cv;

  CallAnalysisValue() = default;

  bool operator==(const CallAnalysisValue &other) const {
    return cv == other.cv;
  }

  static std::vector<CallAnalysisValue> SplitPhi(llvm::Value *v,
                                                 std::vector<FuncParam> &args) {
    std::vector<CallAnalysisValue> result;
    if (llvm::PHINode *phi = llvm::dyn_cast<llvm::PHINode>(v)) {
      for (auto it = phi->op_begin(); it != phi->op_end(); ++it) {
        auto vec = SplitPhi(*it, args);
        for (CallAnalysisValue &value : vec) {
          result.push_back(std::move(value));
        }
      }
    } else if (llvm::CallInst *c = llvm::dyn_cast<llvm::CallInst>(v)) {
      auto vec = CallInst::SplitPhi(c, args);
      for (CallInst &value : vec) {
        result.emplace_back();
        result.back().cv = value;
      }
    } else if (llvm::Argument *arg = llvm::dyn_cast<llvm::Argument>(v)) {
      result.emplace_back();
      for (auto &a : args) {
        if (a.arg == arg) {
          result.back().cv = a;
          break;
        }
      }
    } else if (llvm::ConstantPointerNull *_constNull =
                   llvm::dyn_cast<llvm::ConstantPointerNull>(v)) {
      // skip NULL
    } else if (llvm::Function *fn = llvm::dyn_cast<llvm::Function>(v)) {
      result.emplace_back();
      result.back().cv = Function{fn};
    } else {
      llvm::errs() << "invalid value: \n";
      v->dump();
      v->getType()->dump();
      exit(-1);
    }
    return result;
  }

  std::string ToString() const;
};

class CallAnalysis {
public:
  struct FunctionWithArgs {
    struct Hasher {
      size_t operator()(const FunctionWithArgs &c) const {
        size_t hash = Function::Hasher()(c.fn) + c.callerLineLoc;
        for (auto &f : c.calleeArgs) {
          hash += Function::Hasher()(f);
        }
        return hash;
      }
    };
    Function fn;
    unsigned callerLineLoc;
    std::deque<Function> calleeArgs;

    FunctionWithArgs() = default;
    explicit FunctionWithArgs(Function &fn, unsigned callerLineLoc,
                              const std::deque<Function> &calleeArgs)
        : fn(fn), callerLineLoc(callerLineLoc), calleeArgs(calleeArgs) {}

    bool operator==(const FunctionWithArgs &other) const {
      bool equal = fn == other.fn &&
                   calleeArgs.size() == other.calleeArgs.size() &&
                   callerLineLoc == other.callerLineLoc;
      if (!equal) {
        return false;
      }
      for (unsigned i = 0; i < calleeArgs.size(); ++i) {
        if (!(calleeArgs[i] == other.calleeArgs[i])) {
          return false;
        }
      }
      return true;
    }
  };

public:
  Function::CallCtxMap fnCallCtxMap;
  std::set<std::pair<unsigned, Function>> actuallyCallSet;
  std::unordered_map<FunctionWithArgs, Function::Set, FunctionWithArgs::Hasher>
      cachedFnWithArgsResults;

#ifdef ANALYSIS_INTERNAL
  unsigned stackFrameId{0};
#endif
public:
  void Run(llvm::Module &M);
  void PrintCallCtxMap() const;
  void Clear() {
    fnCallCtxMap.clear();
    actuallyCallSet.clear();
    cachedFnWithArgsResults.clear();
  }
};

} // namespace call_analysis