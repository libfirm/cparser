/*
 * This file is part of cparser.
 * Copyright (C) 2007-2009 Matthias Braun <matze@braunis.de>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */
#include "config.h"

#include "type_t.h"
#include "types.h"
#include "entity_t.h"
#include "ast_t.h"
#include "parser.h"
#include "builtins.h"
#include "lang_features.h"

static entity_t *create_builtin_function(builtin_kind_t kind, const char *name,
                                         type_t *function_type)
{
	symbol_t *const symbol = symbol_table_insert(name);
	entity_t *const entity = allocate_entity_zero(ENTITY_FUNCTION, NAMESPACE_NORMAL);
	entity->declaration.storage_class          = STORAGE_CLASS_EXTERN;
	entity->declaration.declared_storage_class = STORAGE_CLASS_EXTERN;
	entity->declaration.type                   = function_type;
	entity->declaration.implicit               = true;
	entity->base.symbol                        = symbol;
	entity->base.source_position               = builtin_source_position;

	entity->function.btk                       = kind;

	record_entity(entity, /*is_definition=*/false);
	return entity;
}

void create_gnu_builtins(void)
{
#define GNU_BUILTIN(a, b) create_builtin_function(bk_gnu_builtin_##a, "__builtin_" #a, b)

	GNU_BUILTIN(alloca,         make_function_1_type(type_void_ptr, type_size_t));
	GNU_BUILTIN(huge_val,       make_function_0_type(type_double));
	GNU_BUILTIN(huge_valf,      make_function_0_type(type_float));
	GNU_BUILTIN(huge_vall,      make_function_0_type(type_long_double));
	GNU_BUILTIN(inf,            make_function_0_type(type_double));
	GNU_BUILTIN(inff,           make_function_0_type(type_float));
	GNU_BUILTIN(infl,           make_function_0_type(type_long_double));
	GNU_BUILTIN(nan,            make_function_1_type(type_double, type_char_ptr));
	GNU_BUILTIN(nanf,           make_function_1_type(type_float, type_char_ptr));
	GNU_BUILTIN(nanl,           make_function_1_type(type_long_double, type_char_ptr));
	GNU_BUILTIN(va_end,         make_function_1_type(type_void, type_valist));
	GNU_BUILTIN(expect,         make_function_2_type(type_long, type_long, type_long));
	GNU_BUILTIN(return_address, make_function_1_type(type_void_ptr, type_unsigned_int));
	GNU_BUILTIN(frame_address,  make_function_1_type(type_void_ptr, type_unsigned_int));
	GNU_BUILTIN(ffs,            make_function_1_type(type_int, type_unsigned_int));
	GNU_BUILTIN(clz,            make_function_1_type(type_int, type_unsigned_int));
	GNU_BUILTIN(ctz,            make_function_1_type(type_int, type_unsigned_int));
	GNU_BUILTIN(popcount,       make_function_1_type(type_int, type_unsigned_int));
	GNU_BUILTIN(parity,         make_function_1_type(type_int, type_unsigned_int));
	GNU_BUILTIN(prefetch,       make_function_1_type_variadic(type_float, type_void_ptr));
	GNU_BUILTIN(trap,           make_function_type(type_void, 0, NULL, DM_NORETURN));
	GNU_BUILTIN(object_size,    make_function_2_type(type_size_t, type_void_ptr, type_int));
	GNU_BUILTIN(abort,          make_function_type(type_void, 0, NULL, DM_NORETURN));
	GNU_BUILTIN(abs,            make_function_type(type_int, 1, (type_t *[]) { type_int }, DM_CONST));
	GNU_BUILTIN(labs,           make_function_type(type_long, 1, (type_t *[]) { type_long }, DM_CONST));
	GNU_BUILTIN(llabs,          make_function_type(type_long_long, 1, (type_t *[]) { type_long_long }, DM_CONST));
	GNU_BUILTIN(memcpy,         make_function_type(type_void_ptr, 3, (type_t *[]) { type_void_ptr_restrict, type_const_void_ptr_restrict, type_size_t }, DM_NONE));
	GNU_BUILTIN(__memcpy_chk,   make_function_type(type_void_ptr, 4, (type_t *[]) { type_void_ptr_restrict, type_const_void_ptr_restrict, type_size_t, type_size_t}, DM_NONE));
	GNU_BUILTIN(memcmp,         make_function_type(type_int, 3, (type_t *[]) { type_const_void_ptr, type_const_void_ptr, type_size_t }, DM_PURE));
	GNU_BUILTIN(memset,         make_function_type(type_void_ptr, 3, (type_t *[]) { type_void_ptr, type_int, type_size_t }, DM_NONE));
	GNU_BUILTIN(__memset_chk,   make_function_type(type_void_ptr, 4, (type_t *[]) { type_void_ptr, type_int, type_size_t, type_size_t }, DM_NONE));
	GNU_BUILTIN(memmove,        make_function_type(type_void_ptr, 3, (type_t *[]) { type_void_ptr_restrict, type_const_void_ptr_restrict, type_size_t }, DM_NONE));
	GNU_BUILTIN(__memmove_chk,  make_function_type(type_void_ptr, 4, (type_t *[]) { type_void_ptr_restrict, type_const_void_ptr_restrict, type_size_t, type_size_t }, DM_NONE));
	GNU_BUILTIN(strcat,         make_function_type(type_char_ptr, 2, (type_t *[]) { type_char_ptr_restrict, type_const_char_ptr_restrict }, DM_NONE));
	GNU_BUILTIN(__strcat_chk,   make_function_type(type_char_ptr, 3, (type_t *[]) { type_char_ptr_restrict, type_const_char_ptr_restrict, type_size_t }, DM_NONE));
	GNU_BUILTIN(strncat,        make_function_type(type_char_ptr, 3, (type_t *[]) { type_char_ptr_restrict, type_const_char_ptr_restrict, type_size_t }, DM_NONE));
	GNU_BUILTIN(__strncat_chk,  make_function_type(type_char_ptr, 4, (type_t *[]) { type_char_ptr_restrict, type_const_char_ptr_restrict, type_size_t, type_size_t }, DM_NONE));
	GNU_BUILTIN(strlen,         make_function_type(type_size_t, 1, (type_t *[]) { type_const_char_ptr }, DM_PURE));
	GNU_BUILTIN(strcmp,         make_function_type(type_int, 2, (type_t *[]) { type_const_char_ptr, type_const_char_ptr }, DM_PURE));
	GNU_BUILTIN(strcpy,         make_function_type(type_char_ptr, 2, (type_t *[]) { type_char_ptr_restrict, type_const_char_ptr_restrict }, DM_NONE));
	GNU_BUILTIN(__strcpy_chk,   make_function_type(type_char_ptr, 3, (type_t *[]) { type_char_ptr_restrict, type_const_char_ptr_restrict, type_size_t }, DM_NONE));
	GNU_BUILTIN(stpcpy,         make_function_type(type_char_ptr, 2, (type_t *[]) { type_char_ptr_restrict, type_const_char_ptr_restrict }, DM_NONE));
	GNU_BUILTIN(__stpcpy_chk,   make_function_type(type_char_ptr, 3, (type_t *[]) { type_char_ptr_restrict, type_const_char_ptr_restrict, type_size_t }, DM_NONE));
	GNU_BUILTIN(strncpy,        make_function_type(type_char_ptr, 3, (type_t *[]) { type_char_ptr_restrict, type_const_char_ptr_restrict, type_size_t }, DM_NONE));
	GNU_BUILTIN(__strncpy_chk,  make_function_type(type_char_ptr, 4, (type_t *[]) { type_char_ptr_restrict, type_const_char_ptr_restrict, type_size_t, type_size_t }, DM_NONE));
	GNU_BUILTIN(exit,           make_function_type(type_void, 1, (type_t *[]) { type_int }, DM_NORETURN));
	GNU_BUILTIN(malloc,         make_function_type(type_void_ptr, 1, (type_t *[]) { type_size_t }, DM_MALLOC));

	/* TODO: gcc has a LONG list of builtin functions (nearly everything from
	 * C89-C99 and others. Complete this */

#undef GNU_BUILTIN
}

static const char *get_builtin_replacement_name(builtin_kind_t kind)
{
	switch (kind) {
	case bk_gnu_builtin___memcpy_chk:    return "memcpy";
	case bk_gnu_builtin___memmove_chk:   return "memmove";
	case bk_gnu_builtin___memset_chk:    return "memset";
	case bk_gnu_builtin___snprintf_chk:  return "snprintf";
	case bk_gnu_builtin___sprintf_chk:   return "sprintf";
	case bk_gnu_builtin___stpcpy_chk:    return "stpcpy";
	case bk_gnu_builtin___strcat_chk:    return "strcat";
	case bk_gnu_builtin___strcpy_chk:    return "strcpy";
	case bk_gnu_builtin___strncat_chk:   return "strncat";
	case bk_gnu_builtin___strncpy_chk:   return "strncpy";
	case bk_gnu_builtin___vsnprintf_chk: return "vsnprintf";
	case bk_gnu_builtin___vsprintf_chk:  return "vsprintf";
	case bk_gnu_builtin_abort:           return "abort";
	case bk_gnu_builtin_abs:             return "abs";
	case bk_gnu_builtin_exit:            return "exit";
	case bk_gnu_builtin_labs:            return "labs";
	case bk_gnu_builtin_llabs:           return "llabs";
	case bk_gnu_builtin_malloc:          return "malloc";
	case bk_gnu_builtin_memcmp:          return "memcmp";
	case bk_gnu_builtin_memcpy:          return "memcpy";
	case bk_gnu_builtin_memmove:         return "memmove";
	case bk_gnu_builtin_memset:          return "memset";
	case bk_gnu_builtin_snprintf:        return "snprintf";
	case bk_gnu_builtin_sprintf:         return "sprintf";
	case bk_gnu_builtin_stpcpy:          return "stpcpy";
	case bk_gnu_builtin_strcat:          return "strcat";
	case bk_gnu_builtin_strcmp:          return "strcmp";
	case bk_gnu_builtin_strcpy:          return "strcpy";
	case bk_gnu_builtin_strlen:          return "strlen";
	case bk_gnu_builtin_strncat:         return "strncat";
	case bk_gnu_builtin_strncpy:         return "strncpy";
	case bk_gnu_builtin_vsnprintf:       return "vsnprintf";
	case bk_gnu_builtin_vsprintf:        return "vsprintf";

	default:
		break;
	}
	return NULL;
}

int get_builtin_chk_arg_pos(builtin_kind_t kind)
{
	switch (kind) {
	case bk_gnu_builtin___sprintf_chk:
	case bk_gnu_builtin___strcat_chk:
	case bk_gnu_builtin___strcpy_chk:
	case bk_gnu_builtin___vsprintf_chk:
		return 2;
	case bk_gnu_builtin___memcpy_chk:
	case bk_gnu_builtin___memmove_chk:
	case bk_gnu_builtin___memset_chk:
	case bk_gnu_builtin___snprintf_chk:
	case bk_gnu_builtin___strncat_chk:
	case bk_gnu_builtin___strncpy_chk:
	case bk_gnu_builtin___vsnprintf_chk:
		return 3;
	default:
		break;
	}
	return -1;
}

entity_t *get_builtin_replacement(const entity_t *builtin_entity)
{
	builtin_kind_t  kind        = builtin_entity->function.btk;
	const char     *replacement = get_builtin_replacement_name(kind);
	if (replacement == NULL)
		return NULL;

	entity_t *const entity = allocate_entity_zero(ENTITY_FUNCTION, NAMESPACE_NORMAL);
	entity->base.symbol           = symbol_table_insert(replacement);
	entity->base.source_position  = builtin_source_position;
	entity->declaration.storage_class          = STORAGE_CLASS_EXTERN;
	entity->declaration.declared_storage_class = STORAGE_CLASS_EXTERN;
	entity->declaration.type      = builtin_entity->declaration.type;
	entity->declaration.implicit  = true;
	entity->declaration.modifiers = builtin_entity->declaration.modifiers;

	return entity;
}

void create_microsoft_intrinsics(void)
{
#define MS_BUILTIN(a, b) create_builtin_function(bk_ms##a, #a, b)

	/* intrinsics for all architectures */
	MS_BUILTIN(_rotl,                  make_function_2_type(type_unsigned_int,   type_unsigned_int, type_int));
	MS_BUILTIN(_rotr,                  make_function_2_type(type_unsigned_int,   type_unsigned_int, type_int));
	MS_BUILTIN(_rotl64,                make_function_2_type(type_unsigned_int64, type_unsigned_int64, type_int));
	MS_BUILTIN(_rotr64,                make_function_2_type(type_unsigned_int64, type_unsigned_int64, type_int));
	MS_BUILTIN(_byteswap_ushort,       make_function_1_type(type_unsigned_short, type_unsigned_short));
	MS_BUILTIN(_byteswap_ulong,        make_function_1_type(type_unsigned_long,  type_unsigned_long));
	MS_BUILTIN(_byteswap_uint64,       make_function_1_type(type_unsigned_int64, type_unsigned_int64));

	MS_BUILTIN(__debugbreak,            make_function_0_type(type_void));
	MS_BUILTIN(_ReturnAddress,          make_function_0_type(type_void_ptr));
	MS_BUILTIN(_AddressOfReturnAddress, make_function_0_type(type_void_ptr));
	MS_BUILTIN(__popcount,              make_function_1_type(type_unsigned_int, type_unsigned_int));

	/* x86/x64 only */
	MS_BUILTIN(_enable,                make_function_0_type(type_void));
	MS_BUILTIN(_disable,               make_function_0_type(type_void));
	MS_BUILTIN(__inbyte,               make_function_1_type(type_unsigned_char, type_unsigned_short));
	MS_BUILTIN(__inword,               make_function_1_type(type_unsigned_short, type_unsigned_short));
	MS_BUILTIN(__indword,              make_function_1_type(type_unsigned_long, type_unsigned_short));
	MS_BUILTIN(__outbyte,              make_function_2_type(type_void, type_unsigned_short, type_unsigned_char));
	MS_BUILTIN(__outword,              make_function_2_type(type_void, type_unsigned_short, type_unsigned_short));
	MS_BUILTIN(__outdword,             make_function_2_type(type_void, type_unsigned_short, type_unsigned_long));
	MS_BUILTIN(__ud2,                  make_function_type(type_void, 0, NULL, DM_NORETURN));
	MS_BUILTIN(_BitScanForward,        make_function_2_type(type_unsigned_char, type_unsigned_long_ptr, type_unsigned_long));
	MS_BUILTIN(_BitScanReverse,        make_function_2_type(type_unsigned_char, type_unsigned_long_ptr, type_unsigned_long));
	MS_BUILTIN(_InterlockedExchange,   make_function_2_type(type_long, type_long_ptr, type_long));
	MS_BUILTIN(_InterlockedExchange64, make_function_2_type(type_int64, type_int64_ptr, type_int64));

	if (machine_size <= 32) {
		MS_BUILTIN(__readeflags,           make_function_0_type(type_unsigned_int));
		MS_BUILTIN(__writeeflags,          make_function_1_type(type_void, type_unsigned_int));
	} else {
		MS_BUILTIN(__readeflags,           make_function_0_type(type_unsigned_int64));
		MS_BUILTIN(__writeeflags,          make_function_1_type(type_void, type_unsigned_int64));
	}

#undef MS_BUILTIN
}
