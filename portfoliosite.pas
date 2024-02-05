unit PortfolioSite;

{$mode objfpc}

interface

uses
  Classes, SysUtils, webrouter, browserconsole, WebTerm, Web, bulma, TabSystem,
  Widgets, libjquery, jsterm, VFSApp, PortfolioVM, strutils;

type

  { TPortfolioSite }

  TPortfolioSite = class(TRouteObject)
  private
    procedure onCommand(command: string; term: TJQuery);
    procedure ExploreApp(term: TJQuery);
    procedure DoAboutMe;
    procedure DoExploreApp;
    procedure RunVM(URL: string);
  public
    procedure HandleRoute(const URL: String; Params: TStrings); override;
  end;

implementation

{ TPortfolioSite }

procedure TPortfolioSite.onCommand(command: string; term: TJQuery);
var
  cmd: string;
begin
  cmd:=getToken(command);
  case cmd of
    'whoami': term.Echo(term.Name);
    'level': term.Echo('Terminal Level: '+IntToStr(term.Level));
    'token': term.Echo('Terminal Token: '+term.Token(False));
  else
    term.Echo('Uh oh.')
  end;
end;

procedure TPortfolioSite.ExploreApp(term: TJQuery);
var
  params: TTerminalState;
begin
  params:=SimpleState('explore', 'Explore>');
  {term.Enabled:=False;}
  term.Echo('Welcome to the Explore app!');
  term.Echo('Hopefully this is not too overboard.');
  term.Push(@onCommand, params);
end;

procedure TPortfolioSite.DoAboutMe;
begin
  TabSys.ActiveTab:=AboutTab;
  AboutMe(Nil);
end;

procedure TPortfolioSite.DoExploreApp;
begin
  StartWebTerm;
  StartTermApp(@ExploreApp);
end;

procedure TPortfolioSite.RunVM(URL: string);
var
  wh: string;
  vm: TSysRunner;
begin
  wh:=Copy2SymbDel(URL, ':');
  vm:=TSysRunner.Create(Nil, '');
  vm.RunFile(URL, wh);
end;

procedure TPortfolioSite.HandleRoute(const URL: String; Params: TStrings);
begin
  {$IFDEF DEBUG}
  Writeln('Route URL: '+URL);
  {$ENDIF}
  case URL of
    '/webterm': StartWebTerm;
    '/console': TJSHTMLElement(document.getElementById('pasjsconsole')).hidden:=False;
    '/aboutme': DoAboutMe;
    '/explore': DoExploreApp;
    '/vfs': StartVFSTerm;
  else
    RunVM(URL);
  end;
end;

end.

