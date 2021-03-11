-record(ephp_array,
        {size = 0 :: non_neg_integer(),
         values = [] :: [{mixed(), mixed()}],
         last_num_index = 0 :: non_neg_integer(),
         cursor = 1 :: pos_integer() | false}).

-define(IS_ARRAY(A), is_record(A, ephp_array)).
