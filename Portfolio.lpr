program Portfolio;

{$mode objfpc}

uses
  browserconsole, browserapp, JS, Classes, SysUtils, Web, bulma, WebTerm,
  webrouter, PortfolioSite, TabSystem, Widgets, VFSApp, PortfolioVM;

type

  { TMyPortfolio }

  TMyPortfolio = class(TBrowserApplication)
  private
    function TermClick(aEvent: TJSMouseEvent): Boolean;
  protected
    procedure doRun; override;
  end;

{ TMyPortfolio }

procedure TMyPortfolio.doRun;
begin
  GetHTMLElement('TerminalBtn').onclick:=@TermClick;
  GetHTMLElement('pasjsconsole').hidden:=True;
  InitWidgets;
  InitTabs(@ShowWelcome);
  Router.InitHistory(hkHash);
  Router.RegisterRoute('*', TPortfolioSite, True);
  if Router.RouteFromURL = '' then
  begin
    AboutSiteTab:=TabSys.AddTab('About Site', 'AboutSiteTab', @AboutSite);
    TabSys.renderHTML;
    AboutMe(Nil);
  end;
end;

function TMyPortfolio.TermClick(aEvent: TJSMouseEvent): Boolean;
begin
  StartWebTerm;
end;

var
  Application : TMyPortfolio;

begin
  Application:=TMyPortfolio.Create(nil);
  Application.Initialize;
  Application.Run;
end.

