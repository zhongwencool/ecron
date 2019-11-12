defmodule TitanEcronLogger do
  @moduledoc false

  require Logger

  @events [
    [:ecron, :activate],
    [:ecron, :deactivate],
    [:ecron, :delete],
    [:ecron, :success],
    [:ecron, :failure],
    [:ecron, :global, :up],
    [:ecron, :global, :down],
  ]

  def attach(), do: :telemetry.attach_many(:ecron_metrics, @events, &handle_event/4, :undefined)
  def detach(), do: :telemetry.detach(:ecron_metrics)

  def handle_event([:ecron, :activate], %{action_ms: time}, %{name: name, mfa: mfa}, _config) do
    Logger.info("Activate ecron #{inspect name} #{inspect mfa}  at #{inspect time}")
  end

  def handle_event([:ecron, :deactivate], %{action_ms: time}, %{name: name}, _config) do
    Logger.info("Deactivate ecron #{inspect name} at #{inspect time}")
  end

  def handle_event([:ecron, :delete], %{action_ms: time}, %{name: name}, _config) do
    Logger.info("Delete ecron #{inspect name} at #{inspect time}")
  end

  def handle_event([:ecron, :success], %{run_microsecond: ms, run_result: res}, %{name: name, mfa: mfa}, _config) do
    Logger.info(
      "Ecron #{inspect name}(#{inspect mfa}) completed in #{inspect ms} microsecond. result is #{inspect res}"
    )
  end

  def handle_event([:ecron, :failure], %{run_microsecond: ms, run_result: res}, %{name: name, mfa: mfa}, _config) do
    Logger.error("Ecron #{inspect name}(#{inspect mfa}) CRASH in #{inspect ms} microsecond. result is #{inspect res}")
  end

  def handle_event([:ecron, :global, :up], measurements, %{self: node}, _config) do
    %{action_ms: ms, quorum_size: quorum_size, good_nodes: good_nodes, bad_nodes: bad_nodes} = measurements
    Logger.info(
      "Ecron global UP on #{inspect node} at #{inspect ms} ms, quorum_size is #{quorum_size} good_nodes is #{
        inspect good_nodes
      }} bad_nodes is #{inspect bad_nodes}."
    )
  end

  def handle_event([:ecron, :global, :down], measurements, %{self: node}, _config) do
    %{action_ms: ms, quorum_size: quorum_size, good_nodes: good_nodes, bad_nodes: bad_nodes} = measurements
    Logger.info(
      "Ecron global Down on #{inspect node} at #{inspect ms} ms, quorum_size is #{quorum_size} good_nodes is #{
        inspect good_nodes
      }} bad_nodes is #{inspect bad_nodes}."
    )
  end

end
