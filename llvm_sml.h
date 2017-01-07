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

void *llvm_array_type(void *, int);
void *llvm_pointer_type(void *);
void *llvm_qualified_pointer_type(void *, int);
void *llvm_vector_type(void *, int);
void *llvm_element_type(void *);
int llvm_array_length(void *);
int llvm_address_space(void *);
int llvm_vector_size(void *);

void *llvm_void_type(void *);
void *llvm_label_type(void *);
void *llvm_x86_mmx_type(void *);
void *llvm_type_by_name(void *, const char *);

int llvm_classify_value(void *);
void *llvm_type_of(void *);
const char *llvm_value_name(void *);
void llvm_set_value_name(const char *, void *);
void llvm_dump_value(void *);
const char *llvm_string_of_llvalue(void *);
void llvm_replace_all_uses_with(void *, void *);

void *llvm_use_begin(void *);
void *llvm_use_succ(void *);
void *llvm_user(void *);
void *llvm_used_value(void *);

void *llvm_operand(void *, int);
void *llvm_operand_use(void *, int);
void llvm_set_operand(void *, int, void *);
int llvm_num_operands(void *);

int llvm_is_constant(void *);
void *llvm_const_null(void *);
void *llvm_const_all_ones(void *);
void *llvm_const_pointer_null(void *);
void *llvm_undef(void *);
int llvm_is_null(void *);
int llvm_is_undef(void *);
int llvm_constexpr_get_opcode(void *);

int llvm_has_metadata(void *);
void *llvm_metadata(void *, int);
void llvm_set_metadata(void *, int, void *);
void llvm_clear_metadata(void *, int);

void *llvm_mdstring(void *, const char *);
void *llvm_mdnode(void *, void **, int);
void *llvm_mdnull(void *);
const char *llvm_get_mdstring(void *);
void **llvm_get_mdnode_operands(void *, int *);
void **llvm_get_namedmd(void *, const char *, int *);
void llvm_append_namedmd(void *, const char *, void *);

void *llvm_const_int(void *, int);
void *llvm_const_of_int64(void *, long long, int);
long long *llvm_int64_of_const(void *);
void *llvm_const_int_of_string(void *, const char *, int);
void *llvm_const_float(void *, double);
double *llvm_float_of_const(void *);
void *llvm_const_float_of_string(void *, const char *);

void *llvm_const_string(void *, const char*);
void *llvm_const_stringz(void *, const char *);
void *llvm_const_array(void *, void **, int);
void *llvm_const_struct(void *, void **, int);
void *llvm_const_named_struct(void *, void **, int);
void *llvm_const_packed_struct(void *, void **, int);
void *llvm_const_vector(void **, int);
const char *llvm_string_of_const(void *);
void *llvm_const_element(void *, int);
