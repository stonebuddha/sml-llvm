structure Llvm :> LLVM =
struct

structure Core = LlvmCore
structure Bitwriter = LlvmBitwriter
structure Target = LlvmTarget
structure Executionengine = LlvmExecutionengine

end
