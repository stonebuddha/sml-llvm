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

void *llvm_align_of(void *);
void *llvm_size_of(void *);
void *llvm_const_neg(void *);
void *llvm_const_nsw_neg(void *);
void *llvm_const_nuw_neg(void *);
void *llvm_const_fneg(void *);
void *llvm_const_not(void *);
void *llvm_const_add(void *, void *);
void *llvm_const_nsw_add(void *, void *);
void *llvm_const_nuw_add(void *, void *);
void *llvm_const_fadd(void *, void *);
void *llvm_const_sub(void *, void *);
void *llvm_const_nsw_sub(void *, void *);
void *llvm_const_nuw_sub(void *, void *);
void *llvm_const_fsub(void *, void *);
void *llvm_const_mul(void *, void *);
void *llvm_const_nsw_mul(void *, void *);
void *llvm_const_nuw_mul(void *, void *);
void *llvm_const_fmul(void *, void *);
void *llvm_const_udiv(void *, void *);
void *llvm_const_sdiv(void *, void *);
void *llvm_const_exact_sdiv(void *, void *);
void *llvm_const_fdiv(void *, void *);
void *llvm_const_urem(void *, void *);
void *llvm_const_srem(void *, void *);
void *llvm_const_frem(void *, void *);
void *llvm_const_and(void *, void *);
void *llvm_const_or(void *, void *);
void *llvm_const_xor(void *, void *);
void *llvm_const_icmp(int, void *, void *);
void *llvm_const_fcmp(int, void *, void *);
void *llvm_const_shl(void *, void *);
void *llvm_const_lshr(void *, void *);
void *llvm_const_ashr(void *, void *);
void *llvm_const_gep(void *, void **, int);
void *llvm_const_in_bounds_gep(void *, void **, int);
void *llvm_const_trunc(void *, void *);
void *llvm_const_sext(void *, void *);
void *llvm_const_zext(void *, void *);
void *llvm_const_fptrunc(void *, void *);
void *llvm_const_fpext(void *, void *);
void *llvm_const_uitofp(void *, void *);
void *llvm_const_sitofp(void *, void *);
void *llvm_const_fptoui(void *, void *);
void *llvm_const_fptosi(void *, void *);
void *llvm_const_ptrtoint(void *, void *);
void *llvm_const_inttoptr(void *, void *);
void *llvm_const_bitcast(void *, void *);
void *llvm_const_zext_or_bitcast(void *, void *);
void *llvm_const_sext_or_bitcast(void *, void *);
void *llvm_const_trunc_or_bitcast(void *, void *);
void *llvm_const_pointercast(void *, void *);
void *llvm_const_intcast(void *, void *, int);
void *llvm_const_fpcast(void *, void *);
void *llvm_const_select(void *, void *, void *);
void *llvm_const_extractelement(void *, void *);
void *llvm_const_insertelement(void *, void *, void *);
void *llvm_const_shufflevector(void *, void *, void *);
void *llvm_const_extractvalue(void *, int *, int);
void *llvm_const_insertvalue(void *, void *, int *, int);
void *llvm_const_inline_asm(void *, const char *, const char *, int, int);
void *llvm_block_address(void *, void *);

void *llvm_global_parent(void *);
int llvm_is_declaration(void *);
int llvm_linkage(void *);
void llvm_set_linkage(int, void *);
int llvm_unnamed_addr(void *);
void llvm_set_unnamed_addr(int, void *);
const char *llvm_section(void *);
void llvm_set_section(const char *, void *);
int llvm_visibility(void *);
void llvm_set_visibility(int, void *);
int llvm_dll_storage_class(void *);
void llvm_set_dll_storage_class(int, void *);
int llvm_alignment(void *);
void llvm_set_alignment(int, void *);
int llvm_is_global_constant(void *);
void llvm_set_global_constant(int, void *);

void *llvm_declare_global(void *, const char *, void *);
void *llvm_declare_qualified_global(void *, const char *, int, void *);
void *llvm_define_global(const char *, void *, void *);
void *llvm_define_qualified_global(const char *, void *, int, void *);
void *llvm_lookup_global(const char *, void *);
void llvm_delete_global(void *);
void *llvm_global_initializer(void *);
void llvm_set_initializer(void *, void *);
void llvm_remove_initializer(void *);
int llvm_is_thread_local(void *);
void llvm_set_thread_local(int, void *);
int llvm_thread_local_mode(void *);
void llvm_set_thread_local_mode(int, void *);
int llvm_is_externally_initialized(void *);
void llvm_set_externally_initialized(int, void *);
void *llvm_global_begin(void *, int *);
void *llvm_global_succ(void *, int *);
void *llvm_global_end(void *, int *);
void *llvm_global_pred(void *, int *);

void *llvm_add_alias(void *, void *, void *, const char *);

void *llvm_declare_function(const char *, void *, void *);
void *llvm_define_function(const char *, void *, void *);
void *llvm_lookup_function(const char *, void *);
void llvm_delete_function(void *);
int llvm_is_intrinsic(void *);
int llvm_function_call_conv(void *);
void llvm_set_function_call_conv(int, void *);
const char *llvm_gc(void *);
void llvm_set_gc(const char *, void *);
void *llvm_function_begin(void *, int *);
void *llvm_function_succ(void *, int *);
void *llvm_function_end(void *, int *);
void *llvm_function_pred(void *, int *);
void llvm_add_function_attr(void *, int);
void llvm_remove_function_attr(void *, int);
int llvm_function_attr(void *);
void llvm_add_target_dependent_function_attr(void *, const char *, const char *);

void **llvm_params(void *, int *);
void *llvm_param(void *, int);
int llvm_param_attr(void *);
void *llvm_param_parent(void *);
void llvm_add_param_attr(void *, int);
void llvm_remove_param_attr(void *, int);
void llvm_set_param_alignment(void *, int);

void *llvm_value_of_block(void *);
int llvm_value_is_block(void *);
void *llvm_block_of_value(void *);
void *llvm_block_parent(void *);
void **llvm_basic_blocks(void *, int *);
void *llvm_entry_block(void *);
void llvm_delete_block(void *);
void llvm_remove_block(void *);
void llvm_move_block_before(void *, void *);
void llvm_move_block_after(void *, void *);
void *llvm_append_block(void *, const char *, void *);
void *llvm_insert_block(void *, const char *, void *);
void *llvm_block_terminator(void *);

void *llvm_instr_parent(void *);
int llvm_instr_opcode(void *);
int*llvm_icmp_predicate(void *);
int*llvm_fcmp_predicate(void *);
void *llvm_instr_clone(void *);

int llvm_instruction_call_conv(void *);
void llvm_set_instruction_call_conv(int, void *);
void llvm_add_instruction_param_attr(void *, int, int);
void llvm_remove_instruction_param_attr(void *, int, int);

int llvm_is_tail_call(void *);
void llvm_set_tail_call(int, void *);

int llvm_is_volatile(void *);
void llvm_set_volatile(int, void *);

void *llvm_successor(void *, int);
void llvm_set_successor(void *, int, void *);
int llvm_num_successors(void *);

void *llvm_condition(void *);
void llvm_set_condition(void *, void *);
int llvm_is_conditional(void *);

void llvm_add_incoming(void *, void *, void *);
void llvm_delete_instruction(void *);

void *llvm_builder(void *);
void *llvm_insertion_block(void *);
void llvm_insert_into_builder(void *, const char *, void *);

void llvm_set_current_debug_location(void *, void *);
void llvm_clear_current_debug_location(void *);
void *llvm_current_debug_location(void *);
void llvm_set_inst_debug_location(void *, void *);

void *llvm_build_ret_void(void *);
void *llvm_build_ret(void *, void *);
void *llvm_build_aggregate_ret(void **, int, void *);
void *llvm_build_br(void *, void *);
void *llvm_build_cond_br(void *, void *, void *);
void *llvm_build_switch(void *, void *, int, void *);
void *llvm_build_malloc(void *, const char *, void *);
void *llvm_build_array_malloc(void *, void *, const char *);
void *llvm_build_free(void *, void *);
void llvm_add_case(void *, void *, void *);
void *llvm_switch_default_dest(void *);
void *llvm_build_indirect_br(void *, int, void *);
void llvm_add_destination(void *, void *);
void *llvm_build_invoke(void *, void **, int);
void *llvm_build_landingpad(void *, void *, int, const char *);
void llvm_set_cleanup(void *, int);
void llvm_add_clause(void *, void *);
void *llvm_build_resume(void *, void *);
void *llvm_build_unreachable(void *);

void *llvm_build_add(void *, void *, const char *, void *);
void *llvm_build_nsw_add(void *, void *, const char *, void *);
void *llvm_build_nuw_add(void *, void *, const char *, void *);
void *llvm_build_fadd(void *, void *, const char *, void *);
void *llvm_build_sub(void *, void *, const char *, void *);
void *llvm_build_nsw_sub(void *, void *, const char *, void *);
void *llvm_build_nuw_sub(void *, void *, const char *, void *);
void *llvm_build_fsub(void *, void *, const char *, void *);
void *llvm_build_mul(void *, void *, const char *, void *);
void *llvm_build_nsw_mul(void *, void *, const char *, void *);
void *llvm_build_nuw_mul(void *, void *, const char *, void *);
void *llvm_build_fmul(void *, void *, const char *, void *);
void *llvm_build_udiv(void *, void *, const char *, void *);
void *llvm_build_sdiv(void *, void *, const char *, void *);
void *llvm_build_exact_sdiv(void *, void *, const char *, void *);
void *llvm_build_fdiv(void *, void *, const char *, void *);
void *llvm_build_urem(void *, void *, const char *, void *);
void *llvm_build_srem(void *, void *, const char *, void *);
void *llvm_build_frem(void *, void *, const char *, void *);
void *llvm_build_shl(void *, void *, const char *, void *);
void *llvm_build_lshr(void *, void *, const char *, void *);
void *llvm_build_ashr(void *, void *, const char *, void *);
void *llvm_build_and(void *, void *, const char *, void *);
void *llvm_build_or(void *, void *, const char *, void *);
void *llvm_build_xor(void *, void *, const char *, void *);
void *llvm_build_neg(void *, const char *, void *);
void *llvm_build_nsw_neg(void *, const char *, void *);
void *llvm_build_nuw_neg(void *, const char *, void *);
void *llvm_build_fneg(void *, const char *, void *);
void *llvm_build_not(void *, const char *, void *);

void *llvm_build_alloca(void *, const char *, void *);
void *llvm_build_array_alloca(void *, void *, const char *);
void *llvm_build_load(void *, const char *, void *);
void *llvm_build_store(void *, void *, void *);
void *llvm_build_atomicrmw(int, void *);
void *llvm_build_gep(void *, void **, int, const char *, void *);
const char *llvm_build_in_bounds_gep(void *, void **, int);
void *llvm_build_struct_gep(void *, int, const char *, void *);
void *llvm_build_global_string(const char *, const char *, void *);
void *llvm_build_global_stringptr(const char *, const char *, void *);

void *llvm_build_trunc(void *, void *, const char *, void *);
void *llvm_build_zext(void *, void *, const char *, void *);
void *llvm_build_sext(void *, void *, const char *, void *);
void *llvm_build_fptoui(void *, void *, const char *, void *);
void *llvm_build_fptosi(void *, void *, const char *, void *);
void *llvm_build_uitofp(void *, void *, const char *, void *);
void *llvm_build_sitofp(void *, void *, const char *, void *);
void *llvm_build_fptrunc(void *, void *, const char *, void *);
void *llvm_build_fpext(void *, void *, const char *, void *);
void *llvm_build_ptrtoint(void *, void *, const char *, void *);
void *llvm_build_inttoptr(void *, void *, const char *, void *);
void *llvm_build_bitcast(void *, void *, const char *, void *);
void *llvm_build_zext_or_bitcast(void *, void *, const char *);
void *llvm_build_sext_or_bitcast(void *, void *, const char *);
void *llvm_build_trunc_or_bitcast(void *, void *, const char *);
void *llvm_build_pointercast(void *, void *, const char *, void *);
void *llvm_build_intcast(void *, void *, const char *, void *);
void *llvm_build_fpcast(void *, void *, const char *, void *);

void *llvm_build_icmp(int, void *, void *, const char *, void *);
void *llvm_build_fcmp(int, void *, void *, const char *, void *);

void *llvm_build_empty_phi(void *, const char *, void *);
void *llvm_build_call(void *, void **, int, const char *, void *);
void *llvm_build_select(void *, void *, void *, const char *, void *);
void *llvm_build_va_arg(void *, void *, const char *, void *);
void *llvm_build_extractelement(void *, void *, const char *, void *);
void *llvm_build_insertelement(void *, void *, void *, const char *, void *);
void *llvm_build_shufflevector(void *, void *, void *, const char *, void *);
void *llvm_build_extractvalue(void *, int, const char *, void *);
void *llvm_build_insertvalue(void *, void *, int, const char *, void *);
void *llvm_build_is_null(void *, const char *, void *);
void *llvm_build_is_not_null(void *, const char *, void *);
void *llvm_build_ptrdiff(void *, void *, const char *, void *);
