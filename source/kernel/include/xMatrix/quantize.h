#pragma once

fp16_t* convert_fp32_into_fp16_within_view(const struct ViewInstruction*, single_float*);
single_float* convert_fp16_into_fp32_within_view(const struct ViewInstruction*, fp16_t*);

fp16_t* convert_fp32_into_fp16(int, single_float*);
single_float* convert_fp16_into_fp32(int, fp16_t*);

