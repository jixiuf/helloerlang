只实现了gen_server中的handle_call handle_cast ,并且目前不支持timeout

{ok,P}=test:start_link().

gen_server_2:call(P,hello).
gen_server_2:call(P,world).
gen_server_2:call(P,terminate).

gen_server_2:cast(P,hello).
gen_server_2:cast(P,world).
