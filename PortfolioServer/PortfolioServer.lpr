program PortfolioServer;

{$mode objfpc}{$H+}

uses
  cthreads, fphttpapp, PortfolioApp, PortfolioScript;

begin
  Application.Title:='Portfolio Server';
  Application.Port:=8081;
  Application.Threaded:=True;
  Application.Initialize;
  Application.Run;
end.

