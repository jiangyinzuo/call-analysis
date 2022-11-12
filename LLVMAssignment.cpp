//===- Hello.cpp - Example code from "Writing an LLVM Pass" ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements two versions of the LLVM "Hello World" pass described
// in docs/WritingAnLLVMPass.html
//
//===----------------------------------------------------------------------===//

#include <cstddef>
#include <cstdlib>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/ToolOutputFile.h>

#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Utils.h>

#include "llvm/IR/Instructions.h"
#include <llvm/IR/Function.h>
#include <llvm/Pass.h>
#include <llvm/Support/raw_ostream.h>

#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <unordered_map>
#include <unordered_set>
#include <variant>

using namespace llvm;
static ManagedStatic<LLVMContext> GlobalContext;
static LLVMContext &getGlobalContext() { return *GlobalContext; }
/* In LLVM 5.0, when  -O0 passed to clang , the functions generated with clang
 * will have optnone attribute which would lead to some transform passes
 * disabled, like mem2reg.
 */
struct EnableFunctionOptPass : public FunctionPass {
  static char ID;
  EnableFunctionOptPass() : FunctionPass(ID) {}
  bool runOnFunction(Function &F) override {
    if (F.hasFnAttribute(Attribute::OptimizeNone)) {
      F.removeFnAttr(Attribute::OptimizeNone);
    }
    return true;
  }
};

char EnableFunctionOptPass::ID = 0;

///!TODO TO BE COMPLETED BY YOU FOR ASSIGNMENT 2
/// Updated 11/10/2017 by fargo: make all functions
/// processed by mem2reg before this pass.
struct FuncPtrPass : public ModulePass {
  static char ID; // Pass identification, replacement for typeid
  FuncPtrPass() : ModulePass(ID) {}

  static std::vector<Value *> SplitPhi(Value *v) {
    std::vector<Value *> result;
    if (PHINode *phi = dyn_cast<PHINode>(v)) {
      for (unsigned i = 0; i < v->getNumUses(); ++i) {
        auto vec = SplitPhi(phi->op_begin()[i]);
        for (auto &value : vec) {
          result.push_back(value);
        }
      }
    } else {
      result.push_back(v);
    }
    return result;
  }

  struct FnCall {
    std::vector<Value *> operands;

    static void genFnCall(const std::vector<std::vector<Value *>> &operandsVec,
                          unsigned level, std::vector<Value *> &operands,
                          std::vector<FnCall> &output) {
      if (level == operandsVec.size()) {
        output.emplace_back(operands);
        return;
      }
      for (auto *v : operandsVec[level]) {
        operands.push_back(v);
        genFnCall(operandsVec, level + 1, operands, output);
        operands.pop_back();
      }
    }

    static std::vector<FnCall> SplitPhi(CallInst *c) {
      std::vector<std::vector<Value *>> operandsVec;
      operandsVec.push_back(FuncPtrPass::SplitPhi(c->getCalledOperand()));
      for (auto it = c->op_begin(); it != c->op_end(); ++it) {
        operandsVec.push_back(FuncPtrPass::SplitPhi(it->get()));
      }

      std::vector<FnCall> result;
      std::vector<Value *> ops;
      genFnCall(operandsVec, 0, ops, result);
      assert(ops.empty());
      return result;
    }

    explicit FnCall() = default;
    // explicit FnCall(std::vector<Value *> &&operands) : operands(operands) {}
    explicit FnCall(CallInst *c) {
      operands.push_back(c->getCalledOperand());
      for (unsigned i = 0; i < c->arg_size(); ++i) {
        operands.push_back(c->getArgOperand(i));
      }
    }

    bool Determined() const {
      assert(!operands.empty());
      bool result = dyn_cast<Function>(operands[0]);
      for (unsigned i = 1; i < operands.size(); ++i) {
        auto ty = operands[i]->getType();
        result &= !ty->isPointerTy();
      }
      return result;
    }

    std::string ToString() {
      std::string result = "{";
      for (auto &a : operands) {
        result += a->getName();
        result += ", ";
      }
      result += "determined=";
      result += Determined() ? "true" : "false";
      result += "}";
      return result;
    }
  };

  struct FnReturn {
    enum class Type { Call, Value } ty;

    // union
    FnCall call;
    Value *v;

    static std::vector<FnReturn> SplitPhi(ReturnInst *retInst) {
      std::vector<FnReturn> result;
      Value *v = retInst->getReturnValue();
      
      if (CallInst *c = dyn_cast<CallInst>(v)) {
        auto vec = FnCall::SplitPhi(c);
        for (auto &v : vec) {

        }
      }
    }
    explicit FnReturn(CallInst *c) : ty(Type::Call), call(c) {}

    explicit FnReturn(Value *v) : ty(Type::Value), v(v) {}

    bool determined() const {
      bool isCall = ty == Type::Call;
      if (isCall) {
        return call.Determined();
      } else {
        return dyn_cast<Function>(v);
      }
    }

    std::string ToString() {
      bool isCall = ty == Type::Call;
      std::string result = isCall ? "Call{" : "Value{";
      if (isCall) {
        result += call.ToString();
      } else {
        result += v->getName();
        result += ", ";
      }
      result += "determined=";
      result += determined() ? "true" : "false";
      result += "}";
      return result;
    }
  };

  using ArgMap = std::unordered_map<Argument *, BlockMap>;
  std::unordered_map<Function *, std::vector<FnReturn>> fnRetMap;
  std::unordered_map<CallInst *, std::vector<FnCall>> callMap;
  ArgMap argMap;

  bool runOnModule(Module &M) override {
    for (auto it = M.begin(); it != M.end(); it++) {
      Function &f = *it;
      for (BasicBlock &block : f.getBasicBlockList()) {
        for (auto &inst : block.instructionsWithoutDebug()) {
          if (ReturnInst *retInst = dyn_cast<ReturnInst>(&inst)) {
            if (f.getReturnType()->isPointerTy()) {
              Value *v = retInst->getReturnValue();
              if (CallInst *callInst = dyn_cast<CallInst>(v)) {
                fnRetMap[&f].emplace_back(callInst);
              } else {
                fnRetMap[&f].emplace_back(v);
              }
              // SaveValueFunctor svf;
              // visitAllPossibleValue(v, &block, svf);
              // for (auto [_, values] : svf.values) {
              //   for (auto value : values) {
              //     fnRetMap[&f].push_back(value);
              //   }
              // }
              // assert(!fnRetMap[&f].empty());
            }
          } else if (CallInst *callInst = dyn_cast<CallInst>(&inst)) {
            callMap[callInst].emplace_back(callInst);
          }
        }
      }
    }
#ifdef ASSIGNMENT_VIEW
    errs() << "[fnRetMap]\n";
    for (auto &[k, vec] : fnRetMap) {
      errs() << k->getName() << ": ";
      for (auto &v : vec) {
        errs() << v.ToString() << " ";
      }
      errs() << '\n';
    }
    errs() << "\n[callMap]\n";
    for (auto &[k, call] : callMap) {
      for (auto &c : call) {
        errs() << c.ToString();
      }
      errs() << "\n";
    }
#endif

    // for (auto it = M.begin(); it != M.end(); it++) {
    //   Function &f = *it;
    //   for (BasicBlock &block : f.getBasicBlockList()) {
    //     for (auto &inst : block.instructionsWithoutDebug()) {
    //       if (CallInst *callInst = dyn_cast<CallInst>(&inst);
    //           callInst && callInst->getDebugLoc().getLine()) {
    //         unsigned argidx = 0;
    //         for (auto it = callInst->arg_begin(); it != callInst->arg_end();
    //              it++, argidx++) {
    //           Type *argTy = it->get()->getType();
    //           if (argTy->isPointerTy() || argTy->isFunctionTy()) {
    //             assert(it->get());
    //             SaveValueFunctor callees;
    //             iterCalleeValue(callInst, &block, callees);

    //             for (auto [_bb, values] : callees.values) {
    //               for (Value *v : values) {
    //                 Function *callee = dyn_cast<Function>(v);
    //                 assert(callee);
    //                 if (argidx >= callee->arg_size()) { // consider vararg
    //                   break;
    //                 }
    //                 Argument *argument = callee->getArg(argidx);
    //                 AppendArgValueFunctor aavf(argMap[argument]);
    //                 visitAllPossibleValue(it->get(), &block, aavf);
    //               }
    //             }
    //           }
    //         }
    //       }
    //     }
    //   }
    // }

    // for (auto it = M.begin(); it != M.end(); it++) {
    //   Function &f = *it;
    //   for (BasicBlock &block : f.getBasicBlockList()) {
    //     for (auto &inst : block.instructionsWithoutDebug()) {
    //       if (CallInst *callInst = dyn_cast<CallInst>(&inst);
    //           callInst && callInst->getDebugLoc().getLine()) {

    //         errs() << inst.getDebugLoc().getLine() << ' ';
    //         Value *cV = callInst->getCalledOperand();
    //         PrintValueFunctor pvf(argMap);
    //         visitAllPossibleValue(cV, &block, pvf);
    //         pvf.print();
    //       }
    //     }
    //   }
    // }
    // errs() << "Hello: ";
    // errs().write_escaped(M.getName()) << '\n';
    // M.dump();
    // errs() << "------------------------------\n";
    return false;
  }

  template <class HandleFn>
  void visitAllPossibleValue(Value *v, BasicBlock *curBB, HandleFn &handleFn) {
    if (PHINode *phi = dyn_cast<PHINode>(v)) {
      for (unsigned i = 0; i < phi->getNumOperands(); ++i) {
        visitAllPossibleValue(phi->getOperand(i), phi->block_begin()[i],
                              handleFn);
      }
    } else if (Function *f = dyn_cast<Function>(v)) {
      handleFn(curBB, f);
    } else if (Argument *arg = dyn_cast<Argument>(v)) {
      handleFn(curBB, arg);
    } else if (CallInst *c = dyn_cast<CallInst>(v)) {
      // iterCalleeValue(c, curBB, [&](BasicBlock *bb, Function *f) {
      //   auto &frets = fnRetMap[f];
      //   assert(!frets.empty());
      //   for (Value *fret : frets) {
      //     visitAllPossibleValue(fret, bb, handleFn); // argument
      //   }
      // });
    } else if (v->getType()->isPointerTy()) {
      handleFn(curBB, v);
    } else {
      errs() << "unimplemented: ";
      v->dump();
      errs() << v->getName() << '\n';
      v->getType()->dump();
      exit(-1);
    }
  }

  // template <class HandleFunctionFn>
  // void iterCalleeValue(CallInst *c, BasicBlock *curBB,
  //                      HandleFunctionFn &&handle) {
  //   SaveValueFunctor svf;
  //   visitAllPossibleValue(c->getCalledOperand(), curBB, svf);
  //   for (auto &[bb, values] : svf.values) {
  //     for (Value *v : values) {
  //       Function *f = dyn_cast<Function>(v);
  //       assert(f);
  //       handle(bb, f);
  //     }
  //   }
  // }

  // struct PrintValueFunctor {
  //   ArgMap &argMap;
  //   std::unordered_set<std::string> outputs;

  //   explicit PrintValueFunctor(ArgMap &argmap) : argMap(argmap) {}
  //   void operator()(BasicBlock *curBB, Value *v) {
  //     if (Argument *arg = dyn_cast<Argument>(v)) {
  //       bool outputed = false;
  //       for (auto &[bb, values] : argMap[arg]) {
  //         if (curBB == bb) {
  //           for (Value *cv : values) {
  //             outputed = true;
  //             this->operator()(bb, cv);
  //           }
  //         }
  //       }
  //       if (!outputed) {
  //         for (auto &[bb, values] : argMap[arg]) {
  //           for (Value *cv : values) {
  //             this->operator()(bb, cv);
  //           }
  //         }
  //       }
  //     } else {
  //       auto s = v->getName().trim();
  //       if (!s.empty()) {
  //         outputs.insert(s);
  //       }
  //     }
  //   }

  //   void print() {
  //     unsigned outputCount = 0;
  //     for (auto &s : outputs) {
  //       const char *ch = outputCount++ == 0 ? ": " : ", ";
  //       errs() << ch << s;
  //     }
  //     errs() << '\n';
  //   }
  // };

  // struct SaveValueFunctor {
  //   BlockMap values;
  //   void operator()(BasicBlock *bb, Value *v) { values[bb].push_back(v); }
  // };

  // struct AppendArgValueFunctor {
  //   BlockMap &values;
  //   AppendArgValueFunctor(BlockMap &values) : values(values) {}
  //   void operator()(BasicBlock *bb, Value *v) { values[bb].push_back(v); }
  // };
};

char FuncPtrPass::ID = 0;
static RegisterPass<FuncPtrPass> X("funcptrpass",
                                   "Print function call instruction");

static cl::opt<std::string>
    InputFilename(cl::Positional, cl::desc("<filename>.bc"), cl::init(""));

int main(int argc, char **argv) {
  LLVMContext &Context = getGlobalContext();
  SMDiagnostic Err;
  // Parse the command line to read the Inputfilename
  cl::ParseCommandLineOptions(
      argc, argv, "FuncPtrPass \n My first LLVM too which does not do much.\n");

  // Load the input module
  std::unique_ptr<Module> M = parseIRFile(InputFilename, Err, Context);
  if (!M) {
    Err.print(argv[0], errs());
    return 1;
  }

  llvm::legacy::PassManager Passes;

  /// Remove functions' optnone attribute in LLVM5.0
  Passes.add(new EnableFunctionOptPass());
  /// Transform it to SSA
  Passes.add(llvm::createPromoteMemoryToRegisterPass());

  /// Your pass to print Function and Call Instructions
  Passes.add(new FuncPtrPass());
  Passes.run(*M.get());
}
