signature LLVM =
sig

structure Core : LLVM_CORE
structure Bitwriter : LLVM_BITWRITER where type llmodule = Core.llmodule where type llmemorybuffer = Core.llmemorybuffer
structure Target : LLVM_TARGET where type llmodule = Core.llmodule where type lltype = Core.lltype where type llmemorybuffer = Core.llmemorybuffer where type llcontext = Core.llcontext where type llvalue = Core.llvalue where type 'a PassManager.t = 'a Core.PassManager.t
structure Executionengine : LLVM_EXECUTIONENGINE where type llmodule = Target.llmodule where type llvalue = Target.llvalue where type lltype = Target.lltype where type DataLayout.t = Target.DataLayout.t

end
