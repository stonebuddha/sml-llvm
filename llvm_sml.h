void *llvm_create_context(void);
void llvm_dispose_context(void *);
void *llvm_global_context(void);
int llvm_mdkind_id(void *, const char *);

void *llvm_create_module(void *, const char *);
void llvm_dispose_module(void *);
const char *llvm_target_triple(void *);
void llvm_set_target_triple(const char *, void *);
const char *llvm_data_layout(void *);
void llvm_set_data_layout(const char *, void *);
void llvm_dump_module(void *);
void llvm_print_module(const char *, void *);
const char *llvm_string_of_llmodule(void *);
void llvm_set_module_inline_asm(void *, const char *);
void *llvm_get_module_context(void *);
