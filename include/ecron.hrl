-define(MONITOR_WORKER, ecron_monitor).
-define(LocalJob, ecron_local).
-define(GlobalJob, ecron_global).
-define(Ecron, ecron).

%% (16#ffffffff div 1000) 49.71 days.
-define(MAX_TIMEOUT, 4294967).
-define(Success, [ecron, success]).
-define(Failure, [ecron, failure]).
-define(Activate, [ecron, activate]).
-define(Deactivate, [ecron, deactivate]).
-define(Delete, [ecron, delete]).
-define(GlobalUp, [ecron, global, up]).
-define(GlobalDown, [ecron, global, down]).

-record(job, {
    name,
    status = activate,
    job,
    opts = [],
    ok = 0,
    failed = 0,
    aborted = 0,
    link = undefined,
    result = [],
    run_microsecond = []
}).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.
