#ifndef JUMP_TARGET_H
#define JUMP_TARGET_H

#include <libfirm/firm.h>
#include <stdbool.h>

typedef struct jump_target {
 ir_node *block;
 bool     first;
} jump_target;

static inline void init_jump_target(jump_target *const tgt, ir_node *const block)
{
	tgt->block = block;
	tgt->first = false;
}

void jump_from_block_to_target(jump_target *tgt, ir_node *block);

void jump_to_target(jump_target *tgt);

void add_pred_to_jump_target(jump_target *tgt, ir_node *pred);

ir_node *enter_jump_target(jump_target *tgt);

ir_node *get_target_block(jump_target *tgt);

#endif
