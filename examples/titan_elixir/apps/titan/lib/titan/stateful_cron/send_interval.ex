defmodule StatefulCron.SendInterval do
  @moduledoc false

  use GenServer
  @monthly_job_name :monthly_stuff_job

  def start_link(state), do: GenServer.start_link(__MODULE__, state, name: __MODULE__)

  def cancel_workday_stuff(), do: GenServer.call(__MODULE__, :cancel_workday_stuff)

  def cancel_monthly_stuff(), do: :ecron.delete(@monthly_job_name)

  def init([]) do
    ## If this process die, the crontab will auto delete.
    ## It's not necessary to delete job when terminate.
    {:ok, job_ref} = :ecron.send_interval("0 0 8 * * 1-5", self(), :workday_stuff)
    start_time = {{2020, 1, 1}, {0, 0, 0}}
    end_time = {{2022, 1, 1}, {0, 0, 1}}
    :ecron.send_interval(@monthly_job_name, "@monthly", self(), :monthly_stuff, start_time, end_time, [])
    {:ok, %{workday_job: job_ref}}
  end

  def handle_call(:cancel_workday_stuff, _from, state = %{workday_job: job_ref}) do
    :ecron.delete(job_ref)
    {:reply, :ok, %{state | workday_job: :undefined}}
  end
  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(:workday_stuff, state) do
    do_send_workday_stuff(state)
    {:noreply, state}
  end
  def handle_info(:monthly_stuff, state) do
    do_send_monthly_stuff(state)
    {:noreply, state}
  end
  def handle_info(_msg, state) do
    {:noreply, state}
  end

  ## ===================================================================
  ## Internal functions
  ## ===================================================================
  def do_send_workday_stuff(_state) do
    now = :calendar.system_time_to_rfc3339(System.system_time(:second))
    IO.inspect("do workday stuff at #{now}")
  end

  def do_send_monthly_stuff(_State) do
    now = :calendar.system_time_to_rfc3339(System.system_time(:second))
    IO.inspect("Do monthly stuff at #{now}}")
  end

end