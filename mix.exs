defmodule Postgrex.MixProject do
  use Mix.Project

  def project() do
    [
      app: :bencode,
      version: "0.1.0",
      elixir: "~> 1.0",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      description: description(),
      package: package(),
      deps: deps(),
      name: "Bencode",
      source_url: "https://github.com/niamtokik/bencode"
    ]
  end

  def application() do
    []
  end

  defp deps() do
    []
  end

  defp description() do
    "Bencode Erlang Library"
  end


  defp package() do
    [ name: "bencode",
      files: ["rebar.config", "mix.exs", "README.md", "LICENSE",
              "src/*.erl", "src/*.src"],
      maintainers: ["Mathieu Kerjouan"],
      licenses: ["BSD-4"],
      links: %{"GitHub" => "https://github.com/niamtokik/bencode"}
    ]
  end
end
