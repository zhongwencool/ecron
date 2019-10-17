defmodule StatelessCron do
  @moduledoc false

  ## Add job run at 04:00 everyday.
  ## by ecron:add/3
  def add_every_4am_job() do
    job_name = :every_4am_job
    mfa = {__MODULE__, :insepct, ["at 04:00 everyday."]}
    {:ok, ^job_name} = :ecron.add(job_name, "0 4 * * *", mfa)
  end

  ## Add job run at At minute 23 past every 2nd hour from 0 through 20
  ## between {{2020, 1, 1}, {0, 0, 0}} and {{2022, 1, 1}, {0, 0, 0}}
  ## by ecron:add_with_datetime / 4
  def add_limited_start_end_datetime_job2() do
    mfa = {__MODULE__, :inspect, ["at minute 23 past every 2nd hour from 0 through 20."]}
    start_time = {{2020, 1, 1}, {0, 0, 0}}  #datetime or `unlimited`
    end_time = {{2022, 1, 1}, {0, 0, 0}} # datetime or `unlimited`
    :ecron.add_with_datetime("0 23 0-20/2 * * *", mfa, start_time, end_time)
  end

  ## Add job run at 14:15 on day-of-month 1
  ##between {{2020, 1, 1}, {0, 0, 0}} and {{2022, 1, 1}, {0, 0, 0}}
  ## by ecron:add_with_datetime / 5
  def add_limited_start_end_datetime_job() do
    job_name = :limited_start_end_datetime_cron_job
    mfa = {__MODULE__, :inspect, ["at 14:15 on day-of-month 1."]}
    start_time = {{2020, 1, 1}, {0, 0, 0}} # datetime or `unlimited`
    end_time = {{2022, 1, 1}, {0, 0, 0}} # datetime or `unlimited`
    {:ok, ^job_name} = :ecron.add_with_datetime(job_name, "0 15 14 1 * *", mfa, start_time, end_time)
  end

  ## Can only run 100 times at 22:00 on every day-of-week from Monday through Friday.
  ## by ecron:add_with_count / 3
  def add_limited_run_count_job() do
    mfa = {__MODULE__, :inspect, ["at 22:00 on every day-of-week from Monday through Friday."]}
    :ecron.add_with_count("0 22 * * 1-5", mfa, 100)
  end

  ## Can only run 100 times at minute 0 past hour 0 and 12 on day-of-month 1 in every 2nd month.
  ## by ecron:add_with_count/4
  def add_limited_run_count_job2() do
    mfa = {__MODULE__, :inspect, ["at minute 0 past hour 0 and 12 on day-of-month 1 in every 2nd month."]}
    job_name = :limited_run_count_cron_job
    :ecron.add_with_count(job_name, "0 22 * * 1-5", mfa, 100)
  end

  ## Delete a specific task
  def delete_job(job_name) do
    :ok = :ecron.delete(job_name)
  end

  ## MFA
  def inspect(format) do
    now = System.system_time(:second) |> :calendar.system_time_to_rfc3339()
    IO.inspect("#{now} : #{format}")
  end

  ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ## Debug Functions Begin
  ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##
  ## Pause job temporary
  def deactivate_job(job_name) do
    :ecron.deactivate(job_name)
  end

  ## Rerun job.
  def activate_job(job_name) do
    :ecron.activate(job_name)
  end

  ##  Inspect specific statistic
  def job_stats(job_name) do
    :ecron.statistic(job_name)
  end

  ## Inspect all statistic
  def all_job_stats() do
    :ecron.statistic()
  end

  ## Predict latest N datetime.
  def predict_datetime_by_spec(spec, n) do
    :ecron.parse_spec(spec, n)
  end

  ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ## Debug Functions End
  ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

end
