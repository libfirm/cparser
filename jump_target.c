#include "adt/util.h"
#include "jump_target.h"

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

ir_node *get_target_block(jump_target *const tgt)
{
	if (!tgt->block) {
		tgt->block = new_immBlock();
	} else if (tgt->first) {
		ir_node *const jmp = new_r_Jmp(tgt->block);
		tgt->block = new_immBlock();
		tgt->first = false;
		add_immBlock_pred(tgt->block, jmp);
	}
	return tgt->block;
}
