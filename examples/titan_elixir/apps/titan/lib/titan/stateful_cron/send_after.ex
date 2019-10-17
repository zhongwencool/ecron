defmodule StatefulCron.SendAfter do
  @moduledoc false

  use GenServer

  def start_link(state) do
    GenServer.start_link(__MODULE__, state, name: __MODULE__)
  end

  def cancel_weekend_staff() do
    GenServer.call(__MODULE__, :cancel_weekend_staff)
  end

  def init([]) do
    {:ok, ref} = send_after_weekend_msg()
    {:ok, %{timer_ref: ref}}
  end

  def handle_call(:cancel_weekend_staff, _from, state = %{timer_ref: :undefined}) do
    {:reply, :ok, state}
  end
  def handle_call(:cancel_weekend_staff, _from, state = %{timer_ref: ref}) do
    Process.cancel_timer(ref)
    {:reply, :ok, state}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(:weekend_staff, state) do
    do_weekend_staff()
    {:ok, ref} = send_after_weekend_msg()
    {:noreply, %{state | timer_ref: ref}}
  end

  ###===================================================================
  ### Internal functions
  ###===================================================================

  #The timer will be automatically canceled
  #if the given dest is a PID which is not alive or when the given PID no-exits.
  # behaviour as Process.send_after/3
  def send_after_weekend_msg() do
    :ecron.send_after("0 0 20 * * 0,6", self(), :weekend_staff)
  end

  def do_weekend_staff() do
    now = :calendar.system_time_to_rfc3339(System.system_time(:second))
    IO.inspect("do weekday staff at #{now}")
  end

end