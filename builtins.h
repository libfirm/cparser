/*
 * This file is part of cparser.
 * Copyright (C) 2007-2010 Matthias Braun <matze@braunis.de>
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
#ifndef BUILTINS_H
#define BUILTINS_H

#include <stdbool.h>

/**
 * GNU builtin or MS intrinsic functions.
 */
typedef enum builtin_kind_t {
	bk_none = 0,

	bk_gnu_builtin___memcpy_chk,
	bk_gnu_builtin___memmove_chk,
	bk_gnu_builtin___memset_chk,
	bk_gnu_builtin___snprintf_chk,
	bk_gnu_builtin___sprintf_chk,
	bk_gnu_builtin___stpcpy_chk,
	bk_gnu_builtin___strcat_chk,
	bk_gnu_builtin___strcpy_chk,
	bk_gnu_builtin___strncat_chk,
	bk_gnu_builtin___strncpy_chk,
	bk_gnu_builtin___vsnprintf_chk,
	bk_gnu_builtin___vsprintf_chk,
	bk_gnu_builtin_abort,
	bk_gnu_builtin_abs,
	bk_gnu_builtin_alloca,
	bk_gnu_builtin_clz,
	bk_gnu_builtin_ctz,
	bk_gnu_builtin_exit,
	bk_gnu_builtin_expect,
	bk_gnu_builtin_ffs,
	bk_gnu_builtin_frame_address,
	bk_gnu_builtin_huge_val,
	bk_gnu_builtin_huge_valf,
	bk_gnu_builtin_huge_vall,
	bk_gnu_builtin_inf,
	bk_gnu_builtin_inff,
	bk_gnu_builtin_infl,
	bk_gnu_builtin_labs,
	bk_gnu_builtin_llabs,
	bk_gnu_builtin_malloc,
	bk_gnu_builtin_memcmp,
	bk_gnu_builtin_memcpy,
	bk_gnu_builtin_memmove,
	bk_gnu_builtin_memset,
	bk_gnu_builtin_nan,
	bk_gnu_builtin_nanf,
	bk_gnu_builtin_nanl,
	bk_gnu_builtin_object_size,
	bk_gnu_builtin_parity,
	bk_gnu_builtin_popcount,
	bk_gnu_builtin_prefetch,
	bk_gnu_builtin_return_address,
	bk_gnu_builtin_snprintf,
	bk_gnu_builtin_sprintf,
	bk_gnu_builtin_stpcpy,
	bk_gnu_builtin_strcat,
	bk_gnu_builtin_strcmp,
	bk_gnu_builtin_strcpy,
	bk_gnu_builtin_strlen,
	bk_gnu_builtin_strncat,
	bk_gnu_builtin_strncpy,
	bk_gnu_builtin_trap,
	bk_gnu_builtin_va_end,
	bk_gnu_builtin_vsnprintf,
	bk_gnu_builtin_vsprintf,

	bk_ms_AddressOfReturnAddress,
	bk_ms_BitScanForward,
	bk_ms_BitScanReverse,
	bk_ms_InterlockedExchange,
	bk_ms_InterlockedExchange64,
	bk_ms_ReturnAddress,
	bk_ms__debugbreak,
	bk_ms__inbyte,
	bk_ms__indword,
	bk_ms__inword,
	bk_ms__outbyte,
	bk_ms__outdword,
	bk_ms__outword,
	bk_ms__popcount,
	bk_ms__readeflags,
	bk_ms__ud2,
	bk_ms__writeeflags,
	bk_ms_byteswap_uint64,
	bk_ms_byteswap_ulong,
	bk_ms_byteswap_ushort,
	bk_ms_disable,
	bk_ms_enable,
	bk_ms_rotl,
	bk_ms_rotl64,
	bk_ms_rotr,
	bk_ms_rotr64,
} builtin_kind_t;

/**
 * Create predefined gnu builtins.
 */
void create_gnu_builtins(void);

/**
 * Create predefined MS intrinsics.
 */
void create_microsoft_intrinsics(void);

/**
 * Some of the gnu builtins are simply more elaborate declarations of
 * library functions. Return the library function name so we can simply
 * replace the builtins with these during code generation
 */
entity_t *get_builtin_replacement(const entity_t *builtin_entity);

int get_builtin_chk_arg_pos(builtin_kind_t kind);

#endif
