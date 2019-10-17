with Harriet.Sessions;

package Harriet.Repl is

   procedure Read
     (Session : Harriet.Sessions.Harriet_Session;
      Path    : String);

   procedure Execute
     (Session : Harriet.Sessions.Harriet_Session);

end Harriet.Repl;
