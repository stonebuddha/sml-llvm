#ifndef LLVM_SML_CORE_H
#define LLVM_SML_CORE_H

#include "llvm_sml_types.h"

LLVMContextRef llvm_create_context(void);
void llvm_dispose_context(LLVMContextRef);
LLVMContextRef llvm_global_context(void);
unsigned llvm_mdkind_id(LLVMContextRef, const char *);

LLVMModuleRef llvm_create_module(LLVMContextRef, const char *);
void llvm_dispose_module(LLVMModuleRef);
const char *llvm_target_triple(LLVMModuleRef);
void llvm_set_target_triple(const char *, LLVMModuleRef);
const char *llvm_data_layout(LLVMModuleRef);
void llvm_set_data_layout(const char *, LLVMModuleRef);
void llvm_dump_module(LLVMModuleRef);
void llvm_print_module(const char *, LLVMModuleRef);
const char *llvm_string_of_llmodule(LLVMModuleRef);
void llvm_set_module_inline_asm(LLVMModuleRef, const char *);
LLVMContextRef llvm_module_context(LLVMModuleRef);

int llvm_classify_type(LLVMTypeRef);
LLVMContextRef llvm_type_context(LLVMTypeRef);
LLVMBool llvm_type_is_sized(LLVMTypeRef);
void llvm_dump_type(LLVMTypeRef);
const char *llvm_string_of_lltype(LLVMTypeRef);

LLVMTypeRef llvm_i1_type(LLVMContextRef);
LLVMTypeRef llvm_i8_type(LLVMContextRef);
LLVMTypeRef llvm_i16_type(LLVMContextRef);
LLVMTypeRef llvm_i32_type(LLVMContextRef);
LLVMTypeRef llvm_i64_type(LLVMContextRef);
LLVMTypeRef llvm_integer_type(LLVMContextRef, unsigned);
unsigned llvm_integer_bitwidth(LLVMTypeRef);

LLVMTypeRef llvm_float_type(LLVMContextRef);
LLVMTypeRef llvm_double_type(LLVMContextRef);
LLVMTypeRef llvm_x86fp80_type(LLVMContextRef);
LLVMTypeRef llvm_fp128_type(LLVMContextRef);
LLVMTypeRef llvm_ppc_fp128_type(LLVMContextRef);

LLVMTypeRef llvm_function_type(LLVMTypeRef, LLVMTypeRef *, unsigned);
LLVMTypeRef llvm_var_arg_function_type(LLVMTypeRef, LLVMTypeRef *, unsigned);
LLVMBool llvm_is_var_arg(LLVMTypeRef);
LLVMTypeRef llvm_return_type(LLVMTypeRef);
LLVMTypeRef *llvm_param_types(LLVMTypeRef, unsigned *);

LLVMTypeRef llvm_struct_type(LLVMContextRef, LLVMTypeRef *, unsigned);
LLVMTypeRef llvm_packed_struct_type(LLVMContextRef, LLVMTypeRef *, unsigned);
const char *llvm_struct_name(LLVMTypeRef);
LLVMTypeRef llvm_named_struct_type(LLVMContextRef, const char *);
void llvm_struct_set_body(LLVMTypeRef, LLVMTypeRef *, unsigned, LLVMBool);
LLVMTypeRef *llvm_struct_element_types(LLVMTypeRef, unsigned *);
LLVMBool llvm_is_packed(LLVMTypeRef);
LLVMBool llvm_is_opaque(LLVMTypeRef);

LLVMTypeRef llvm_array_type(LLVMTypeRef, unsigned);
LLVMTypeRef llvm_pointer_type(LLVMTypeRef);
LLVMTypeRef llvm_qualified_pointer_type(LLVMTypeRef, unsigned);
LLVMTypeRef llvm_vector_type(LLVMTypeRef, unsigned);
LLVMTypeRef llvm_element_type(LLVMTypeRef);
unsigned llvm_array_length(LLVMTypeRef);
unsigned llvm_address_space(LLVMTypeRef);
unsigned llvm_vector_size(LLVMTypeRef);

LLVMTypeRef llvm_void_type(LLVMContextRef);
LLVMTypeRef llvm_label_type(LLVMContextRef);
LLVMTypeRef llvm_x86_mmx_type(LLVMContextRef);
LLVMTypeRef llvm_type_by_name(LLVMModuleRef, const char *);

int llvm_classify_value(LLVMValueRef);
LLVMTypeRef llvm_type_of(LLVMValueRef);
const char *llvm_value_name(LLVMValueRef);
void llvm_set_value_name(const char *, LLVMValueRef);
void llvm_dump_value(LLVMValueRef);
const char *llvm_string_of_llvalue(LLVMValueRef);
void llvm_replace_all_uses_with(LLVMValueRef, LLVMValueRef);

LLVMUseRef llvm_use_begin(LLVMValueRef);
LLVMUseRef llvm_use_succ(LLVMUseRef);
LLVMValueRef llvm_user(LLVMUseRef);
LLVMValueRef llvm_used_value(LLVMUseRef);

LLVMValueRef llvm_operand(LLVMValueRef, unsigned);
LLVMUseRef llvm_operand_use(LLVMValueRef, unsigned);
void llvm_set_operand(LLVMValueRef, unsigned, LLVMValueRef);
int llvm_num_operands(LLVMValueRef);

LLVMBool llvm_is_constant(LLVMValueRef);
LLVMValueRef llvm_const_null(LLVMTypeRef);
LLVMValueRef llvm_const_all_ones(LLVMTypeRef);
LLVMValueRef llvm_const_pointer_null(LLVMTypeRef);
LLVMValueRef llvm_undef(LLVMTypeRef);
LLVMBool llvm_is_null(LLVMValueRef);
LLVMBool llvm_is_undef(LLVMValueRef);
int llvm_constexpr_get_opcode(LLVMValueRef);

LLVMBool llvm_has_metadata(LLVMValueRef);
LLVMValueRef llvm_metadata(LLVMValueRef, unsigned);
void llvm_set_metadata(LLVMValueRef, unsigned, LLVMValueRef);
void llvm_clear_metadata(LLVMValueRef, unsigned);

LLVMValueRef llvm_mdstring(LLVMContextRef, const char *);
LLVMValueRef llvm_mdnode(LLVMContextRef, LLVMValueRef *, unsigned);
LLVMValueRef llvm_mdnull(LLVMContextRef);
const char *llvm_get_mdstring(LLVMValueRef);
LLVMValueRef *llvm_get_mdnode_operands(LLVMValueRef, unsigned *);
LLVMValueRef *llvm_get_namedmd(LLVMModuleRef, const char *, unsigned *);
void llvm_append_namedmd(LLVMModuleRef, const char *, LLVMValueRef);

LLVMValueRef llvm_const_int(LLVMTypeRef, int);
LLVMValueRef llvm_const_of_int64(LLVMTypeRef, long long, LLVMBool);
long long *llvm_int64_of_const(LLVMValueRef);
LLVMValueRef llvm_const_int_of_string(LLVMTypeRef, const char *, unsigned);
LLVMValueRef llvm_const_float(LLVMTypeRef, double);
double *llvm_float_of_const(LLVMValueRef);
LLVMValueRef llvm_const_float_of_string(LLVMTypeRef, const char *);

LLVMValueRef llvm_const_string(LLVMContextRef, const char*);
LLVMValueRef llvm_const_stringz(LLVMContextRef, const char *);
LLVMValueRef llvm_const_array(LLVMTypeRef, LLVMValueRef *, unsigned);
LLVMValueRef llvm_const_struct(LLVMContextRef, LLVMValueRef *, unsigned);
LLVMValueRef llvm_const_named_struct(LLVMTypeRef, LLVMValueRef *, unsigned);
LLVMValueRef llvm_const_packed_struct(LLVMContextRef, LLVMValueRef *, unsigned);
LLVMValueRef llvm_const_vector(LLVMValueRef *, unsigned);
const char *llvm_string_of_const(LLVMValueRef);
LLVMValueRef llvm_const_element(LLVMValueRef, unsigned);

LLVMValueRef llvm_align_of(LLVMTypeRef);
LLVMValueRef llvm_size_of(LLVMTypeRef);
LLVMValueRef llvm_const_neg(LLVMValueRef);
LLVMValueRef llvm_const_nsw_neg(LLVMValueRef);
LLVMValueRef llvm_const_nuw_neg(LLVMValueRef);
LLVMValueRef llvm_const_fneg(LLVMValueRef);
LLVMValueRef llvm_const_not(LLVMValueRef);
LLVMValueRef llvm_const_add(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_nsw_add(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_nuw_add(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_fadd(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_sub(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_nsw_sub(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_nuw_sub(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_fsub(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_mul(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_nsw_mul(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_nuw_mul(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_fmul(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_udiv(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_sdiv(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_exact_sdiv(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_fdiv(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_urem(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_srem(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_frem(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_and(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_or(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_xor(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_icmp(int, LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_fcmp(int, LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_shl(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_lshr(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_ashr(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_gep(LLVMValueRef, LLVMValueRef *, unsigned);
LLVMValueRef llvm_const_in_bounds_gep(LLVMValueRef, LLVMValueRef *, unsigned);
LLVMValueRef llvm_const_trunc(LLVMValueRef, LLVMTypeRef);
LLVMValueRef llvm_const_sext(LLVMValueRef, LLVMTypeRef);
LLVMValueRef llvm_const_zext(LLVMValueRef, LLVMTypeRef);
LLVMValueRef llvm_const_fptrunc(LLVMValueRef, LLVMTypeRef);
LLVMValueRef llvm_const_fpext(LLVMValueRef, LLVMTypeRef);
LLVMValueRef llvm_const_uitofp(LLVMValueRef, LLVMTypeRef);
LLVMValueRef llvm_const_sitofp(LLVMValueRef, LLVMTypeRef);
LLVMValueRef llvm_const_fptoui(LLVMValueRef, LLVMTypeRef);
LLVMValueRef llvm_const_fptosi(LLVMValueRef, LLVMTypeRef);
LLVMValueRef llvm_const_ptrtoint(LLVMValueRef, LLVMTypeRef);
LLVMValueRef llvm_const_inttoptr(LLVMValueRef, LLVMTypeRef);
LLVMValueRef llvm_const_bitcast(LLVMValueRef, LLVMTypeRef);
LLVMValueRef llvm_const_zext_or_bitcast(LLVMValueRef, LLVMTypeRef);
LLVMValueRef llvm_const_sext_or_bitcast(LLVMValueRef, LLVMTypeRef);
LLVMValueRef llvm_const_trunc_or_bitcast(LLVMValueRef, LLVMTypeRef);
LLVMValueRef llvm_const_pointercast(LLVMValueRef, LLVMTypeRef);
LLVMValueRef llvm_const_intcast(LLVMValueRef, LLVMTypeRef, LLVMBool);
LLVMValueRef llvm_const_fpcast(LLVMValueRef, LLVMTypeRef);
LLVMValueRef llvm_const_select(LLVMValueRef, LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_extractelement(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_insertelement(LLVMValueRef, LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_shufflevector(LLVMValueRef, LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_const_extractvalue(LLVMValueRef, unsigned *, unsigned);
LLVMValueRef llvm_const_insertvalue(LLVMValueRef, LLVMValueRef, unsigned *, unsigned);
LLVMValueRef llvm_const_inline_asm(LLVMTypeRef, const char *, const char *, LLVMBool, LLVMBool);
LLVMValueRef llvm_block_address(LLVMValueRef, LLVMBasicBlockRef);

LLVMModuleRef llvm_global_parent(LLVMValueRef);
LLVMBool llvm_is_declaration(LLVMValueRef);
int llvm_linkage(LLVMValueRef);
void llvm_set_linkage(int, LLVMValueRef);
LLVMBool llvm_unnamed_addr(LLVMValueRef);
void llvm_set_unnamed_addr(LLVMBool, LLVMValueRef);
const char *llvm_section(LLVMValueRef);
void llvm_set_section(const char *, LLVMValueRef);
int llvm_visibility(LLVMValueRef);
void llvm_set_visibility(int, LLVMValueRef);
int llvm_dll_storage_class(LLVMValueRef);
void llvm_set_dll_storage_class(int, LLVMValueRef);
unsigned llvm_alignment(LLVMValueRef);
void llvm_set_alignment(unsigned, LLVMValueRef);
LLVMBool llvm_is_global_constant(LLVMValueRef);
void llvm_set_global_constant(LLVMBool, LLVMValueRef);

LLVMValueRef llvm_declare_global(LLVMTypeRef, const char *, LLVMModuleRef);
LLVMValueRef llvm_declare_qualified_global(LLVMTypeRef, const char *, unsigned, LLVMModuleRef);
LLVMValueRef llvm_define_global(const char *, LLVMValueRef, LLVMModuleRef);
LLVMValueRef llvm_define_qualified_global(const char *, LLVMValueRef, unsigned, LLVMModuleRef);
LLVMValueRef llvm_lookup_global(const char *, LLVMModuleRef);
void llvm_delete_global(LLVMValueRef);
LLVMValueRef llvm_global_initializer(LLVMValueRef);
void llvm_set_initializer(LLVMValueRef, LLVMValueRef);
void llvm_remove_initializer(LLVMValueRef);
LLVMBool llvm_is_thread_local(LLVMValueRef);
void llvm_set_thread_local(LLVMBool, LLVMValueRef);
int llvm_thread_local_mode(LLVMValueRef);
void llvm_set_thread_local_mode(int, LLVMValueRef);
LLVMBool llvm_is_externally_initialized(LLVMValueRef);
void llvm_set_externally_initialized(LLVMBool, LLVMValueRef);
void *llvm_global_begin(LLVMModuleRef, int *);
void *llvm_global_succ(LLVMValueRef, int *);
void *llvm_global_end(LLVMModuleRef, int *);
void *llvm_global_pred(LLVMValueRef, int *);

LLVMValueRef llvm_add_alias(LLVMModuleRef, LLVMTypeRef, LLVMValueRef, const char *);

LLVMValueRef llvm_declare_function(const char *, LLVMTypeRef, LLVMModuleRef);
LLVMValueRef llvm_define_function(const char *, LLVMTypeRef, LLVMModuleRef);
LLVMValueRef llvm_lookup_function(const char *, LLVMModuleRef);
void llvm_delete_function(LLVMValueRef);
LLVMBool llvm_is_intrinsic(LLVMValueRef);
unsigned llvm_function_call_conv(LLVMValueRef);
void llvm_set_function_call_conv(unsigned, LLVMValueRef);
const char *llvm_gc(LLVMValueRef);
void llvm_set_gc(const char *, LLVMValueRef);
void *llvm_function_begin(LLVMModuleRef, int *);
void *llvm_function_succ(LLVMValueRef, int *);
void *llvm_function_end(LLVMModuleRef, int *);
void *llvm_function_pred(LLVMValueRef, int *);
void llvm_add_function_attr(LLVMValueRef, int);
void llvm_remove_function_attr(LLVMValueRef, int);
int llvm_function_attr(LLVMValueRef);
void llvm_add_target_dependent_function_attr(LLVMValueRef, const char *, const char *);

LLVMValueRef *llvm_params(LLVMValueRef, unsigned *);
LLVMValueRef llvm_param(LLVMValueRef, unsigned);
int llvm_param_attr(LLVMValueRef);
LLVMValueRef llvm_param_parent(LLVMValueRef);
LLVMValueRef llvm_param_begin(LLVMValueRef, int *);
LLVMValueRef llvm_param_succ(LLVMValueRef, int *);
LLVMValueRef llvm_param_end(LLVMValueRef, int *);
LLVMValueRef llvm_param_pred(LLVMValueRef, int *);
void llvm_add_param_attr(LLVMValueRef, int);
void llvm_remove_param_attr(LLVMValueRef, int);
void llvm_set_param_alignment(LLVMValueRef, unsigned);

LLVMValueRef llvm_value_of_block(LLVMBasicBlockRef);
LLVMBool llvm_value_is_block(LLVMValueRef);
LLVMBasicBlockRef llvm_block_of_value(LLVMValueRef);
LLVMValueRef llvm_block_parent(LLVMBasicBlockRef);
LLVMBasicBlockRef *llvm_basic_blocks(LLVMValueRef, unsigned *);
LLVMBasicBlockRef llvm_entry_block(LLVMValueRef);
void llvm_delete_block(LLVMBasicBlockRef);
void llvm_remove_block(LLVMBasicBlockRef);
void llvm_move_block_before(LLVMBasicBlockRef, LLVMBasicBlockRef);
void llvm_move_block_after(LLVMBasicBlockRef, LLVMBasicBlockRef);
LLVMBasicBlockRef llvm_append_block(LLVMContextRef, const char *, LLVMValueRef);
LLVMBasicBlockRef llvm_insert_block(LLVMContextRef, const char *, LLVMBasicBlockRef);
void *llvm_block_begin(LLVMValueRef, int *);
void *llvm_block_succ(LLVMBasicBlockRef, int *);
void *llvm_block_end(LLVMValueRef, int *);
void *llvm_block_pred(LLVMBasicBlockRef, int *);
LLVMValueRef llvm_block_terminator(LLVMBasicBlockRef);

LLVMBasicBlockRef llvm_instr_parent(LLVMValueRef);
void *llvm_instr_begin(LLVMBasicBlockRef, int *);
void *llvm_instr_succ(LLVMValueRef, int *);
void *llvm_instr_end(LLVMBasicBlockRef, int *);
void *llvm_instr_pred(LLVMValueRef, int *);
int llvm_instr_opcode(LLVMValueRef);
int *llvm_icmp_predicate(LLVMValueRef);
int *llvm_fcmp_predicate(LLVMValueRef);
LLVMValueRef llvm_instr_clone(LLVMValueRef);

unsigned llvm_instruction_call_conv(LLVMValueRef);
void llvm_set_instruction_call_conv(unsigned, LLVMValueRef);
void llvm_add_instruction_param_attr(LLVMValueRef, unsigned, int);
void llvm_remove_instruction_param_attr(LLVMValueRef, unsigned, int);

LLVMBool llvm_is_tail_call(LLVMValueRef);
void llvm_set_tail_call(LLVMBool, LLVMValueRef);

LLVMBool llvm_is_volatile(LLVMValueRef);
void llvm_set_volatile(LLVMBool, LLVMValueRef);

LLVMBasicBlockRef llvm_successor(LLVMValueRef, unsigned);
void llvm_set_successor(LLVMValueRef, unsigned, LLVMBasicBlockRef);
unsigned llvm_num_successors(LLVMValueRef);

LLVMValueRef llvm_condition(LLVMValueRef);
void llvm_set_condition(LLVMValueRef, LLVMValueRef);
LLVMBool llvm_is_conditional(LLVMValueRef);

void llvm_add_incoming(LLVMValueRef, LLVMBasicBlockRef, LLVMValueRef);
void **llvm_incoming(LLVMValueRef, unsigned *);
void llvm_delete_instruction(LLVMValueRef);

LLVMBuilderRef llvm_builder(LLVMContextRef);
void llvm_position_builder(void *, int, LLVMBuilderRef);
LLVMBasicBlockRef llvm_insertion_block(LLVMBuilderRef);
void llvm_insert_into_builder(LLVMValueRef, const char *, LLVMBuilderRef);

void llvm_set_current_debug_location(LLVMBuilderRef, LLVMValueRef);
void llvm_clear_current_debug_location(LLVMBuilderRef);
LLVMValueRef llvm_current_debug_location(LLVMBuilderRef);
void llvm_set_inst_debug_location(LLVMBuilderRef, LLVMValueRef);

LLVMValueRef llvm_build_ret_void(LLVMBuilderRef);
LLVMValueRef llvm_build_ret(LLVMValueRef, LLVMBuilderRef);
LLVMValueRef llvm_build_aggregate_ret(LLVMValueRef *, unsigned, LLVMBuilderRef);
LLVMValueRef llvm_build_br(LLVMBasicBlockRef, LLVMBuilderRef);
LLVMValueRef llvm_build_cond_br(LLVMValueRef, LLVMBasicBlockRef, LLVMBasicBlockRef, LLVMBuilderRef);
LLVMValueRef llvm_build_switch(LLVMValueRef, LLVMBasicBlockRef, unsigned, LLVMBuilderRef);
LLVMValueRef llvm_build_malloc(LLVMTypeRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_array_malloc(LLVMTypeRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_free(LLVMValueRef, LLVMBuilderRef);
void llvm_add_case(LLVMValueRef, LLVMValueRef, LLVMBasicBlockRef);
LLVMBasicBlockRef llvm_switch_default_dest(LLVMValueRef);
LLVMValueRef llvm_build_indirect_br(LLVMValueRef, unsigned, LLVMBuilderRef);
void llvm_add_destination(LLVMValueRef, LLVMBasicBlockRef);
LLVMValueRef llvm_build_invoke(LLVMValueRef, LLVMValueRef *, unsigned, LLVMBasicBlockRef, LLVMBasicBlockRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_landingpad(LLVMTypeRef, LLVMValueRef, unsigned, const char *, LLVMBuilderRef);
void llvm_set_cleanup(LLVMValueRef, LLVMBool);
void llvm_add_clause(LLVMValueRef, LLVMValueRef);
LLVMValueRef llvm_build_resume(LLVMValueRef, LLVMBuilderRef);
LLVMValueRef llvm_build_unreachable(LLVMBuilderRef);

LLVMValueRef llvm_build_add(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_nsw_add(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_nuw_add(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_fadd(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_sub(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_nsw_sub(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_nuw_sub(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_fsub(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_mul(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_nsw_mul(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_nuw_mul(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_fmul(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_udiv(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_sdiv(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_exact_sdiv(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_fdiv(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_urem(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_srem(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_frem(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_shl(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_lshr(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_ashr(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_and(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_or(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_xor(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_neg(LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_nsw_neg(LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_nuw_neg(LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_fneg(LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_not(LLVMValueRef, const char *, LLVMBuilderRef);

LLVMValueRef llvm_build_alloca(LLVMTypeRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_array_alloca(LLVMTypeRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_load(LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_store(LLVMValueRef, LLVMValueRef, LLVMBuilderRef);
LLVMValueRef llvm_build_atomicrmw(int, LLVMValueRef, LLVMValueRef, int, LLVMBool, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_gep(LLVMValueRef, LLVMValueRef *, unsigned, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_in_bounds_gep(LLVMValueRef, LLVMValueRef *, unsigned, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_struct_gep(LLVMValueRef, unsigned, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_global_string(const char *, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_global_stringptr(const char *, const char *, LLVMBuilderRef);

LLVMValueRef llvm_build_trunc(LLVMValueRef, LLVMTypeRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_zext(LLVMValueRef, LLVMTypeRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_sext(LLVMValueRef, LLVMTypeRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_fptoui(LLVMValueRef, LLVMTypeRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_fptosi(LLVMValueRef, LLVMTypeRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_uitofp(LLVMValueRef, LLVMTypeRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_sitofp(LLVMValueRef, LLVMTypeRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_fptrunc(LLVMValueRef, LLVMTypeRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_fpext(LLVMValueRef, LLVMTypeRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_ptrtoint(LLVMValueRef, LLVMTypeRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_inttoptr(LLVMValueRef, LLVMTypeRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_bitcast(LLVMValueRef, LLVMTypeRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_zext_or_bitcast(LLVMValueRef, LLVMTypeRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_sext_or_bitcast(LLVMValueRef, LLVMTypeRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_trunc_or_bitcast(LLVMValueRef, LLVMTypeRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_pointercast(LLVMValueRef, LLVMTypeRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_intcast(LLVMValueRef, LLVMTypeRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_fpcast(LLVMValueRef, LLVMTypeRef, const char *, LLVMBuilderRef);

LLVMValueRef llvm_build_icmp(int, LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_fcmp(int, LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);

LLVMValueRef llvm_build_phi(LLVMValueRef *, LLVMBasicBlockRef *, unsigned, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_empty_phi(LLVMTypeRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_call(LLVMValueRef, LLVMValueRef *, unsigned, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_select(LLVMValueRef, LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_va_arg(LLVMValueRef, LLVMTypeRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_extractelement(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_insertelement(LLVMValueRef, LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_shufflevector(LLVMValueRef, LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_extractvalue(LLVMValueRef, unsigned, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_insertvalue(LLVMValueRef, LLVMValueRef, unsigned, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_is_null(LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_is_not_null(LLVMValueRef, const char *, LLVMBuilderRef);
LLVMValueRef llvm_build_ptrdiff(LLVMValueRef, LLVMValueRef, const char *, LLVMBuilderRef);

LLVMMemoryBufferRef llvm_memorybuffer_of_file(const char *);
LLVMMemoryBufferRef llvm_memorybuffer_of_stdin(void);
LLVMMemoryBufferRef llvm_memorybuffer_of_string(const char *, const char *);
const char *llvm_memorybuffer_as_string(LLVMMemoryBufferRef);
void llvm_memorybuffer_dispose(LLVMMemoryBufferRef);

LLVMPassManagerRef llvm_passmanager_create(void);
LLVMPassManagerRef llvm_passmanager_create_function(LLVMModuleRef);
LLVMBool llvm_passmanager_run_module(LLVMModuleRef, LLVMPassManagerRef);
LLVMBool llvm_passmanager_initialize(LLVMPassManagerRef);
LLVMBool llvm_passmanager_run_function(LLVMValueRef, LLVMPassManagerRef);
LLVMBool llvm_passmanager_finalize(LLVMPassManagerRef);
void llvm_passmanager_dispose(LLVMPassManagerRef);

#endif
