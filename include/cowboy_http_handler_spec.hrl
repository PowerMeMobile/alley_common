-type opts() :: any().
-type state() :: any().
-type terminate_reason() :: {normal, shutdown}
	| {normal, timeout} %% Only occurs in loop handlers.
	| {error, closed} %% Only occurs in loop handlers.
	| {error, overflow} %% Only occurs in loop handlers.
	| {error, atom()}.

-spec init({atom(), http}, Req, opts())
	-> {ok, Req, state()}
	| {loop, Req, state()}
	| {loop, Req, state(), hibernate}
	| {loop, Req, state(), timeout()}
	| {loop, Req, state(), timeout(), hibernate}
	| {shutdown, Req, state()}
	| {upgrade, protocol, module()}
	| {upgrade, protocol, module(), Req, opts()}
	when Req::cowboy_req:req().
-spec handle(Req, State) -> {ok, Req, State}
	when Req::cowboy_req:req(), State::state().
-spec terminate(terminate_reason(), cowboy_req:req(), state()) -> ok.
