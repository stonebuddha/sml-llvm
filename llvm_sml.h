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
void *llvm_module_context(void *);

int llvm_classify_type(void *);
void *llvm_type_context(void *);
int llvm_type_is_sized(void *);
void llvm_dump_type(void *);
const char *llvm_string_of_lltype(void *);

void *llvm_i1_type(void *);
void *llvm_i8_type(void *);
void *llvm_i16_type(void *);
void *llvm_i32_type(void *);
void *llvm_i64_type(void *);
void *llvm_integer_type(void *, int);
int llvm_integer_bitwidth(void *);

void *llvm_float_type(void *);
void *llvm_double_type(void *);
void *llvm_x86fp80_type(void *);
void *llvm_fp128_type(void *);
void *llvm_ppc_fp128_type(void *);

void *llvm_function_type(void *, void **, int);
void *llvm_var_arg_function_type(void *, void **, int);
int llvm_is_var_arg(void *);
void *llvm_return_type(void *);
void **llvm_param_types(void *, int *);

void *llvm_struct_type(void *, void **, int);
void *llvm_packed_struct_type(void *, void **, int);
const char *llvm_struct_name(void *);
void *llvm_named_struct_type(void *, const char *);
void llvm_struct_set_body(void *, void **, int, int);
void **llvm_struct_element_types(void *, int *);
int llvm_is_packed(void *);
int llvm_is_opaque(void *);
