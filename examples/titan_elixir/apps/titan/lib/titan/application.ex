defmodule Titan.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    :ok = TitanCronLogger.attach()
    children = [
      {StatefulCron.SendInterval, []},
      {StatefulCron.SendAfter, []},
    ]
    opts = [strategy: :one_for_one, name: Titan.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def stop(_state) do
    TitanCronLogger.detach()
    :ok
  end
end
