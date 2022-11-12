#include "CallAnalysis.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Module.h"

namespace call_analysis {

std::optional<Function> CallInst::TryEvaluate() {

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
	if (ty->isFunctionTy()) return true;
	if (auto* pt = llvm::dyn_cast<llvm::PointerType>(ty)) {
		return isFnOrFnPtrTy(pt->getElementType());
	}
	return false;
}

std::vector<CallInst> CallInst::SplitPhi(llvm::CallInst *c) {
  std::vector<std::vector<CallAnalysisValue>> splittedOperands;
  splittedOperands.push_back(
      CallAnalysisValue::SplitPhi(c->getCalledOperand()));
  for (unsigned i = 0; i < c->arg_size(); ++i) {
    llvm::Value *op = c->getOperand(i);
    if (auto ty = op->getType(); isFnOrFnPtrTy(ty)) {
      splittedOperands.push_back(CallAnalysisValue::SplitPhi(op));
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
  for (auto it = M.begin(); it != M.end(); it++) {
    llvm::Function &f = *it;
    for (llvm::BasicBlock &block : f.getBasicBlockList()) {
      for (auto &inst : block.instructionsWithoutDebug()) {
        if (llvm::ReturnInst *retInst =
                llvm::dyn_cast<llvm::ReturnInst>(&inst)) {
          if (auto ty = f.getReturnType();
              ty->isPointerTy() || ty->isPointerTy()) {
            auto *v = retInst->getReturnValue();
            auto splittedCav = CallAnalysisValue::SplitPhi(v);
            lazyEvaledFn[&f] = std::move(splittedCav);
          }
        } else if (llvm::CallInst *callInst =
                       llvm::dyn_cast<llvm::CallInst>(&inst)) {
          auto splittedCallInst = CallInst::SplitPhi(callInst);
          lazyEvaledCallInst[callInst] = std::move(splittedCallInst);
        }
      }
    }
  }

#ifdef ANALYSIS_INTERNAL
  PrintMap();
#endif

  // evaluate callInsts and functions
}

void CallAnalysis::PrintMap() const {
  llvm::errs() << "[lazyEvaledFn]\n";
  for (auto &[fn, vec] : lazyEvaledFn) {
    llvm::errs() << fn->getName() << ":\n";
    for (auto &v : vec) {
      llvm::errs() << "  " << v.ToString() << "\n";
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