# This file is only used by Ecron as a dependency.
# Use rebar3 instead for compiling, running tests, etc.
defmodule Ecron.MixProject do
  use Mix.Project

  {:ok, [{:application, :ecron, props}]} = :file.consult("src/ecron.app.src")
  @props Keyword.take(props, [:applications, :description, :env, :mod, :vsn])

  def application do
    @props
  end

  def project do
    [
      app: :ecron,
      version: to_string(application()[:vsn]),
      language: :erlang,
      deps: [telemetry: "~> 1.3"]
    ]
  end
end
