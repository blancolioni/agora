with GCS.Errors;

with SK.Machine;

with Leander.Environments;
with Leander.Errors;
with Leander.Parser.Modules;
with Leander.Primitives;

with Agora.Paths;

package body Agora.Library is

   Machine : SK.Machine.SK_Machine;

   ------------------
   -- Load_Library --
   ------------------

   procedure Load_Library is
      Env : constant Leander.Environments.Environment :=
              Leander.Parser.Modules.Load_Module
                ("Professions",
                 Agora.Paths.Config_File
                   ("Professions.hs"));
   begin
      Machine := SK.Machine.Create (1024 * 1024);
      Leander.Primitives.Load_SK_Primitives (Machine.all);

      if not GCS.Errors.Has_Errors
        and then not Leander.Errors.Has_Errors
      then
         Env.Compile (Machine);
      end if;

   end Load_Library;

end Agora.Library;
