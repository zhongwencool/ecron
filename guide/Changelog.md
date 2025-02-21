# Changelog

### 1.1.0
### Features

*   **Comprehensive Documentation**: Added extensive documentation using `?MODULEDOC` and `?DOC` macros to explain the module's purpose, functions, parameters, and return values. This includes:
    *   Module-level documentation for `ecron`.
    *   Detailed documentation for each exported function, including `create/3`, `create/4`, `send_after/3`, `send_interval/2`, `send_interval/3`, `send_interval/4`, `delete/1`, `delete/2`, `deactivate/1`, `activate/1`, `deactivate/2`, `activate/2`, `statistic/0`, `statistic/1`, `statistic/2`, `predict_datetime/2`, `predict_datetime/4`, `reload/0`, `reload/1`, and `parse_spec/2`.
    *   Examples in both Erlang and Elixir for most functions.
    *   Clear explanations of options, return values, and potential errors.

*   **Deprecated Functions**: Marked older functions (`add/3`, `add/4`, `add/6`, `add/7`, `add_with_time/5`, `add_with_time/6`, `add_with_count/3`, `add_with_count/5`, `send_interval/5`, `send_interval/7`, `send_interval/8`) as deprecated, advising users to use `create/4` and `send_interval/4` instead.

*   **Enhanced `create/4` Function**:
    *   Added detailed explanations for the `Opts` parameter, including `register`, `start_time`, `end_time`, `singleton`, `max_count`, and `max_runtime_ms`.
    *   Included a note about automatic job removal when `max_count` is reached.
    *   Added a `TimeRange` info box explaining the constraints on `start_time` and `end_time`.
    *   Added an example of handling invalid time configurations.

*   **Enhanced `send_interval/4` Function**:
    *   Added a note indicating that this is a repeatable timer and can be seen in `statistic/0` results.
    *   Clarified that `ecron:delete/1` should be used to cancel the timer, not `erlang:cancel_timer/1`.

*   **Enhanced `statistic/2` Function**:
    *   Provided a detailed explanation of the `statistic()` return value, including descriptions of `ok`, `crashed`, `skipped`, and `aborted` states.

*   **Time Zone Configuration**:
    *   Added documentation for configuring the time zone using the `time_zone` application environment variable.
    *   Explained the difference between `local` and `utc` time zone settings.

*   **Telemetry Integration**:
    *   Documented the use of Telemetry for instrumentation and logging.
    *   Provided a table of Telemetry events, including `success`, `activate`, `deactivate`, `delete`, `crashed`, `skipped`, `aborted`, `global,up`, and `global,down`.
    *   Explained how to enable or disable logging via the `log` configuration.
    *   Provided guidance on writing custom Telemetry event handlers.

*   **Internal Functions**:
    *   Added `?DOC(false)` to internal functions to hide them from the generated documentation.

### Changes

*   **Refactored `add` Functions**: Replaced multiple `add` functions with a unified `create` function for better consistency and maintainability.

*   **Improved Error Handling**: Added more specific error handling and return values for various functions.

*   **Code Clarity**: Improved code readability by adding comments and using more descriptive variable names.

### Deprecations

*   The following functions have been marked as deprecated:
    *   `add/3`, `add/4`, `add/6`, `add/7`
    *   `add_with_time/5`, `add_with_time/6`
    *   `add_with_count/3`, `add_with_count/5`
    *   `send_interval/5`, `send_interval/7`, `send_interval/8`

### Purpose

These changes significantly enhance the usability and maintainability of the `ecron` library by providing comprehensive documentation, improving code structure, and integrating Telemetry for better monitoring and logging. The deprecation of older functions encourages users to adopt the more consistent and feature-rich `create` function.

### 1.0.2
- Use ex_doc to update documentation.

### 1.0.1
- Fix concurrent cron job execution timing issue.

### 1.0.0
Nothing changed, just update version from 0.6.1 to 1.0.0.

### 0.6.1
- fix some spec warning.
- upgrade telemetry to 1.1.0

### 0.5.2
- redefine `[ecron global up/down]` telemetry event.

### 0.5.1
- Replace `cluster_quorum_size` by `global_quorum_size`.
- Check if job's name is duplicate.

### 0.5.0
- Support global_jobs by `global`.
- Add global/up/down telemetry metrics.
 
### 0.4.0

- Add `ecron:reload/0`.
- Expose telemetry metrics. 
