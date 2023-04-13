#pragma once

fp16_t* convert_fp32_into_fp16(const struct ViewInstruction, single_float*);
single_float* convert_fp16_into_fp32(const struct ViewInstruction, fp16_t*);

