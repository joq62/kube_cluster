%% This is the application resource file (.app file) for the 'base'
%% application.
{application, iaas,
[{description, "iaas" },
{vsn, "1.0.0" },
{modules, 
	  [iaas_app,iaas_sup,iaas,
		iaas_lib]},
{registered,[iaas]},
{applications, [kernel,stdlib]},
{mod, {iaas_app,[]}},
{start_phases, []}
]}.
