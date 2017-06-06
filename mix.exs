defmodule ZLists.Mixfile do
  use Mix.Project

  def project do
      [app: :zlists,
       version: "0.0.4",
       elixir: "~> 1.4",
       build_embedded: Mix.env == :prod,
       start_permanent: Mix.env == :prod,
       deps: deps(),
       description: description(),
       package: package(),
       name: "zlists",
       source_url: "https://github.com/vjache/erlang-zlists.git"
       ]
  end

  def application do
      [applications: []]
  end

  defp deps do
	[{:ex_doc, ">= 0.0.0", only: :dev}]
  end

  defp description do
     """
    Z-Lists -- an erlang lazy lists.
    """
  end

  defp package do
    # These are the default files included in the package
        [
	 name: :zlists,
	 files: [
	   "mix.exs",
	   "rebar.config",
	   "src"						 
	 ],
	 maintainers: ["Vyacheslav Vorobyov"],
	 licenses: ["Apache 2.0"],
	 links: %{"GitHub" => "https://github.com/elixir-lang/mydep.git"}
	 ]
  end
end
