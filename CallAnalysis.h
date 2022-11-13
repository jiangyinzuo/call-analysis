#pragma once
#include <cstddef>
#include <llvm/IR/LLVMContext.h>

#include "llvm/IR/Instructions.h"
#include <llvm/IR/Function.h>
#include <llvm/Support/raw_ostream.h>

#include <optional>
#include <unordered_map>
#include <variant>

namespace call_analysis {
struct CallAnalysisValue;

struct Function {
  struct Hasher {
    size_t operator()(const Function &c) const;
  };

  llvm::Function *fn;

  bool operator==(const Function &other) const {
    return fn == other.fn;
  }

  std::string ToString() const {
    std::string result = "[Fn ";
    result += fn->getName();
    result += ']';
    return result;
  }
};

struct FuncArgument {
  struct Hasher {
    size_t operator()(const FuncArgument &c) const;
  };

  unsigned idx;
  llvm::Argument *arg;

  explicit FuncArgument(unsigned idx, llvm::Argument *arg)
      : idx(idx), arg(arg) {}
  
  bool operator==(const FuncArgument &other) const {
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

  std::vector<CallAnalysisValue> operands;

  CallInst() = default;
  CallInst(std::vector<CallAnalysisValue> &&operands) : operands(operands) {}
  CallInst(const std::vector<CallAnalysisValue> &operands)
      : operands(operands) {}
  
  bool operator==(const CallInst &other) const;

  static std::vector<CallInst> SplitPhi(llvm::CallInst *c, std::vector<FuncArgument> &args);

  bool TryEvaluate(const std::vector<Function> &args, std::vector<Function> &results);

  std::string ToString() const;

private:
  friend class CallAnalysisValue;
  static void combineCallInst(
      const std::vector<std::vector<CallAnalysisValue>> &operandsVec,
      unsigned level, std::vector<CallAnalysisValue> &operands,
      std::vector<CallInst> &output);
};

struct CallAnalysisValue {
  struct Hasher {
    size_t operator()(const CallAnalysisValue &c) const;
  };

  std::variant<CallInst, FuncArgument, Function> cv;

  CallAnalysisValue() = default;

  bool operator==(const CallAnalysisValue &other) const {
    return cv == other.cv;
  }

  static std::vector<CallAnalysisValue>
  SplitPhi(llvm::Value *v, std::vector<FuncArgument> &args) {
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

  std::optional<Function> TryEvaluate();
  std::string ToString() const;
};

class CallAnalysis {
  struct FunctionNode {
    std::vector<FuncArgument> args;
    std::vector<CallInst> callees;
    std::vector<CallAnalysisValue> returnValues;
  };
  std::unordered_map<llvm::Function *, FunctionNode> lazyEvaledFn;
  std::unordered_map<llvm::CallInst *, std::vector<CallInst>>
      lazyEvaledCallInst;
  std::unordered_map<CallInst, std::vector<Function>, CallInst::Hasher> evaluatedCallResult;
  
public:
  void Run(llvm::Module &M);
  void PrintMap() const;
  void Clear() {
    lazyEvaledFn.clear();
    lazyEvaledCallInst.clear();
    evaluatedCallResult.clear();
  }
};

} // namespace call_analysis