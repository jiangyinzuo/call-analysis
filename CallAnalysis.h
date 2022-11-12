#pragma once
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
  llvm::Function *fn;

	std::optional<Function> TryEvaluate() const {
		return std::make_optional<Function>(*this);
	}

  std::string ToString() const {
    std::string result = "[Fn ";
    result += fn->getName();
    result += ']';
    return result;
  }
};

struct CallInst {
  std::vector<CallAnalysisValue> operands;
  CallInst() = default;
  CallInst(std::vector<CallAnalysisValue> &&operands) : operands(operands) {}
  CallInst(const std::vector<CallAnalysisValue> &operands)
      : operands(operands) {}

  static std::vector<CallInst> SplitPhi(llvm::CallInst *c);

	std::optional<Function> TryEvaluate();

  std::string ToString() const;

private:
  friend class CallAnalysisValue;
  static void combineCallInst(
      const std::vector<std::vector<CallAnalysisValue>> &operandsVec,
      unsigned level, std::vector<CallAnalysisValue> &operands,
      std::vector<CallInst> &output);
};

struct FuncArgument {
  llvm::Argument *arg;

	std::optional<Function> TryEvaluate() const {
		return std::optional<Function>();
	}

  std::string ToString() const {
    std::string result = "[Arg ";
    result += arg->getName();
    result += ']';
    return result;
  }
};


struct CallAnalysisValue {
  std::variant<CallInst, FuncArgument, Function> cv;

  CallAnalysisValue() = default;
  explicit CallAnalysisValue(llvm::Value *v) {
    if (auto c = llvm::dyn_cast<llvm::CallInst>(v)) {
      llvm::errs() << "unreachable!";
      exit(-1);
    } else if (auto arg = llvm::dyn_cast<llvm::Argument>(v)) {
      cv = FuncArgument{arg};
    } else if (auto fn = llvm::dyn_cast<llvm::Function>(v)) {
      cv = Function{fn};
    } else {
      llvm::errs() << "invalid value: \n";
      v->dump();
      v->getType()->dump();
      exit(-1);
    }
  }
  static std::vector<CallAnalysisValue> SplitPhi(llvm::Value *v) {
    std::vector<CallAnalysisValue> result;
    if (llvm::PHINode *phi = llvm::dyn_cast<llvm::PHINode>(v)) {
      for (auto it = phi->op_begin(); it != phi->op_end(); ++it) {
        auto vec = SplitPhi(*it);
        for (CallAnalysisValue &value : vec) {
          result.push_back(std::move(value));
        }
      }
    } else if (llvm::CallInst *c = llvm::dyn_cast<llvm::CallInst>(v)) {
      auto vec = CallInst::SplitPhi(c);
      for (CallInst &value : vec) {
        CallAnalysisValue cav;
        cav.cv = value;
        result.push_back(std::move(cav));
      }
    } else if (llvm::ConstantPointerNull *_constNull =
                   llvm::dyn_cast<llvm::ConstantPointerNull>(v)) {
      // skip NULL
    } else {
      result.emplace_back(v);
    }
    return result;
  }

	std::optional<Function> TryEvaluate();
  std::string ToString() const;
}; // namespace call_analysis

class CallAnalysis {
  std::unordered_map<llvm::Function *, std::vector<CallAnalysisValue>>
      lazyEvaledFn;
  std::unordered_map<llvm::CallInst *, std::vector<CallInst>>
      lazyEvaledCallInst;

public:
  void Run(llvm::Module &M);
  void PrintMap() const;
};

} // namespace call_analysis