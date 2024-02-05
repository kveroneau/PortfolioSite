unit WebTerm;

{$mode objfpc}

interface

uses
  Classes, SysUtils, libjquery, jsterm, JS, Web, bulma, ajaxlib, jsonlib,
  PortfolioVM;

type

  TTermAppProc = reference to procedure(term: TJQuery);

  { TPortfolioTerm }

  TPortfolioTerm = class(TComponent)
  private
    FModal: TBulmaModal;
    FTerm: TJQuery;
    FRemoteFile: TWebRequest;
    FAuth: TJSONRequest;
    FOnLogin: TLoginEvent;
    FCommand: TWebRequest;
    FEngine: TVMRequest;
    FRunner: TSysRunner;
    procedure onLogin(user, password: string; callback: TLoginEvent);
    procedure onCommand(command: string; term: TJQuery);
    procedure onInit(term: TJQuery);
    procedure onExit(term: TJQuery);
    procedure ToggleConsole;
    function GetElement(aID: string): TJSHTMLElement;
    procedure GetFile(AFile: string);
    procedure DisplayOutput;
    procedure ProcessLogin(data: TJSONData);
    procedure ProcessCommand;
    procedure ProcessVM(vm: TVMData);
    procedure RunnerOutput(s: string);
  public
    property JSTerm: TJQuery read FTerm;
    constructor Create(AOwner: TComponent);
    procedure ShowTerm;
  end;

procedure StartWebTerm;
procedure StartTermApp(app: TTermAppProc);


implementation

var
  PortfolioTerm: TPortfolioTerm;

procedure StartWebTerm;
begin
  if not Assigned(PortfolioTerm) then
    PortfolioTerm:=TPortfolioTerm.Create(Nil)
  else
    PortfolioTerm.ShowTerm;
end;

procedure StartTermApp(app: TTermAppProc);
begin
  app(PortfolioTerm.JSTerm);
end;

{ TPortfolioTerm }

procedure TPortfolioTerm.onLogin(user, password: string; callback: TLoginEvent);
var
  data: TStringList;
begin
  FOnLogin:=callback;
  FTerm.Enabled:=False;
  data:=TStringList.Create;
  with data do
  begin
    Add('username='+encodeURIComponent(user));
    Add('password='+encodeURIComponent(password));
    Add('realm='+encodeURIComponent(FTerm.Name));
  end;
  FAuth.DoRequest(data);
  data.Free;
end;

procedure TPortfolioTerm.onCommand(command: string; term: TJQuery);
var
  cmd: string;
begin
  cmd:=getToken(command);
  case cmd of
    'ver': term.Echo('Portfolio Command Shell v0.1');
    'echo': term.Echo(getToken(command));
    'hide': GetElement(getToken(command)).hidden:=True;
    'show': GetElement(getToken(command)).hidden:=False;
    'console': ToggleConsole;
    'prompt': term.Prompt:=getToken(command);
    'clear': term.Clear;
    'help': GetFile('termhelp.txt');
    'use': FRunner.RunFile(getToken(command), 'WebBin');
    'run': FEngine.DoRequest('run='+encodeURIComponent(getToken(command)));
    'vfstypes': GetFile('vfstypes.txt');
  else
    FCommand.DoRequest('cmd='+encodeURIComponent(cmd+' '+command)+'&realm='+encodeURIComponent(term.Name));
  end;
end;

procedure TPortfolioTerm.onInit(term: TJQuery);
begin
  term.Echo('For help with the Portfolio WebTerm, type `help`.');
end;

procedure TPortfolioTerm.onExit(term: TJQuery);
begin
  {GetElement('terminal').hidden:=True;}
  term.Echo('Terminal Level: '+IntToStr(term.Level));
end;

procedure TPortfolioTerm.ToggleConsole;
begin
  if GetElement('pasjsconsole').hidden then
    GetElement('pasjsconsole').hidden:=False
  else
    GetElement('pasjsconsole').hidden:=True;
end;

function TPortfolioTerm.GetElement(aID: string): TJSHTMLElement;
begin
  Result:=TJSHTMLElement(document.getElementById(aID));
end;

procedure TPortfolioTerm.GetFile(AFile: string);
begin
  FTerm.Enabled:=False;
  FRemoteFile:=TWebRequest.Create(Self, 'get', AFile);
  FRemoteFile.OnChange:=@DisplayOutput;
  FRemoteFile.DoRequest;
end;

procedure TPortfolioTerm.DisplayOutput;
begin
  if not FRemoteFile.Complete then
    Exit;
  FTerm.Echo(FRemoteFile.responseText);
  FreeAndNil(FRemoteFile);
  FTerm.Enabled:=True;
end;

procedure TPortfolioTerm.ProcessLogin(data: TJSONData);
begin
  FOnLogin(data.Strings['token']);
  FTerm.Enabled:=True;
end;

procedure TPortfolioTerm.ProcessCommand;
begin
  if not FCommand.Complete then
    Exit;
  FTerm.Echo(FCommand.responseText);
  FTerm.Enabled:=True;
end;

procedure TPortfolioTerm.ProcessVM(vm: TVMData);
begin
  FTerm.Echo(vm.command);
end;

procedure TPortfolioTerm.RunnerOutput(s: string);
begin
  FTerm.Echo(s);
end;

constructor TPortfolioTerm.Create(AOwner: TComponent);
var
  params: TJSTerminalOptions;
begin
  inherited Create(AOwner);
  FAuth:=TJSONRequest.Create(Self, 'post', '/Portfolio/Auth');
  FAuth.OnJSON:=@ProcessLogin;
  FCommand:=TWebRequest.Create(Self, 'post', '/Portfolio/Command');
  FCommand.OnChange:=@ProcessCommand;
  FEngine:=TVMRequest.Create(Self, 'post', '/Portfolio/VM');
  FEngine.OnVM:=@ProcessVM;
  FRunner:=TSysRunner.Create(Self, 'PHello From SysRunner!~X');
  FRunner.OnOutput:=@RunnerOutput;
  FModal:=TBulmaModal.Create(Self, 'terminal');
  FModal.ShowModal;
  {params:=LoginTerm('Welcome to my Portfolio Terminal!', 'Portfolio/>', @onLogin);}
  params:=SimpleTerm;
  params.greeting:='Welcome to My Portfolio Terminal!';
  params.prompt:='Portfolio/>';
  FTerm:=InitTerminal('terminal', @onCommand, params, @onInit);
end;

procedure TPortfolioTerm.ShowTerm;
begin
  FModal.ShowModal;
end;

end.

