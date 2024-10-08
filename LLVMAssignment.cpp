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

#include "CallAnalysis.h"

static llvm::ManagedStatic<llvm::LLVMContext> GlobalContext;
static llvm::LLVMContext &getGlobalContext() { return *GlobalContext; }
/* In LLVM 5.0, when  -O0 passed to clang , the functions generated with clang
 * will have optnone attribute which would lead to some transform passes
 * disabled, like mem2reg.
 */
struct EnableFunctionOptPass : public llvm::FunctionPass {
  static char ID;
  EnableFunctionOptPass() : FunctionPass(ID) {}
  bool runOnFunction(llvm::Function &F) override {
    if (F.hasFnAttribute(llvm::Attribute::OptimizeNone)) {
      F.removeFnAttr(llvm::Attribute::OptimizeNone);
    }
    return true;
  }
};

char EnableFunctionOptPass::ID = 0;

///!TODO TO BE COMPLETED BY YOU FOR ASSIGNMENT 2
/// Updated 11/10/2017 by fargo: make all functions
/// processed by mem2reg before this pass.
struct FuncPtrPass : public llvm::ModulePass {
  static char ID; // Pass identification, replacement for typeid
  FuncPtrPass() : ModulePass(ID) {}

public:
  bool runOnModule(llvm::Module &M) override {
#ifdef ANALYSIS_INTERNAL
    llvm::errs() << "[LLVM IR]\n";
    llvm::errs().write_escaped(M.getName()) << '\n';
    M.dump();
    llvm::errs() << "------------------------------\n";
#endif
    call_analysis::CallAnalysis analysis;
    analysis.Run(M);
    return false;
  }
};

char FuncPtrPass::ID = 0;
static llvm::RegisterPass<FuncPtrPass> X("funcptrpass",
                                         "Print function call instruction");

static llvm::cl::opt<std::string> InputFilename(llvm::cl::Positional,
                                                llvm::cl::desc("<filename>.bc"),
                                                llvm::cl::init(""));

int main(int argc, char **argv) {
  llvm::LLVMContext &Context = getGlobalContext();
  llvm::SMDiagnostic Err;
  // Parse the command line to read the Inputfilename
  llvm::cl::ParseCommandLineOptions(
      argc, argv, "FuncPtrPass \n My first LLVM too which does not do much.\n");

  // Load the input module
  std::unique_ptr<llvm::Module> M = parseIRFile(InputFilename, Err, Context);
  if (!M) {
    Err.print(argv[0], llvm::errs());
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
