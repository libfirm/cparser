/*
 * This file is part of cparser.
 * Copyright (C) 2012 Christoph Mallon <christoph.mallon@gmx.de>
 */
#include "jump_target.h"

#include "adt/util.h"

void jump_from_block_to_target(jump_target *const tgt, ir_node *const block)
{
	if (!tgt->block) {
		tgt->block = block;
		tgt->first = true;
		return;
	} else if (tgt->first) {
		ir_node *const jmp = new_r_Jmp(tgt->block);
		tgt->block = new_immBlock();
		tgt->first = false;
		add_immBlock_pred(tgt->block, jmp);
	}
	ir_node *const jmp = new_r_Jmp(block);
	add_immBlock_pred(tgt->block, jmp);
}

void jump_to_target(jump_target *const tgt)
{
	ir_node *const block = get_cur_block();
	if (block)
		jump_from_block_to_target(tgt, block);
}

void add_pred_to_jump_target(jump_target *const tgt, ir_node *const pred)
{
	if (!tgt->block) {
		tgt->block = new_immBlock();
	} else if (tgt->first) {
		ir_node *const jmp = new_r_Jmp(tgt->block);
		tgt->block = new_immBlock();
		tgt->first = false;
		add_immBlock_pred(tgt->block, jmp);
	}
	add_immBlock_pred(tgt->block, pred);
}

ir_node *enter_jump_target(jump_target *const tgt)
{
	ir_node *const block = tgt->block;
	if (block && !tgt->first)
		mature_immBlock(block);
	set_cur_block(block);
	return block;
}

void enter_immature_jump_target(jump_target *const tgt)
{
	ir_node *jmp;
	ir_node *block = tgt->block;
	if (!block) {
		/* Avoid unreachable loops by adding a Bad entry. */
		jmp = new_Bad(mode_X);
		goto new_block;
	} else if (tgt->first) {
		tgt->first = false;
		jmp = new_r_Jmp(block);
new_block:
		tgt->block = block = new_immBlock();
		add_immBlock_pred(block, jmp);
	}
	set_cur_block(block);
}
