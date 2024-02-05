unit PortfolioApp;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, cmdsys, fphttpapp, qvfs,
  StrUtils, PortfolioScript, qvfsjson;

type

  { TPortfolioModule }

  TPortfolioModule = class(TFPWebModule)
    procedure AuthRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure CommandRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure VFSRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure VMRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    FUser, FRealm: string;
    function GetFile(AFile: string): string;
    procedure DoOpCode(op: char);
    function RunScript(AFile, ALoc, params: string): string;
    procedure MainShell(command: string; ARequest: TRequest; AResponse: TResponse);
    procedure LoginShell(command: string; ARequest: TRequest; AResponse: TResponse);
    function UserAuth(const user, pass: string): Boolean;
  public

  end;

var
  PortfolioModule: TPortfolioModule;

implementation

{$R *.lfm}

{ TPortfolioModule }

procedure TPortfolioModule.AuthRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  pass, token: string;
begin
  if ARequest.Method <> 'POST' then
    Exit;
  Handled:=True;
  token:='';
  AResponse.ContentType:='application/json';
  with ARequest.ContentFields do
  begin
    FUser:=Values['username'];
    pass:=Values['password'];
    FRealm:=Values['realm'];
  end;
  if FRealm = 'login' then
  begin
    if FUser = 'demo' then
      token:='DEMO-TOKEN';
  end
  else if FRealm = 'vfs' then
  begin
    if UserAuth(FUser, pass) then
      token:='VFS-TOKEN';
  end;
  AResponse.Content:='{"token":"'+token+'"}';
end;

procedure TPortfolioModule.CommandRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  cmdline, realm: string;
begin
  if ARequest.Method <> 'POST' then
    Exit;
  Handled:=True;
  AResponse.ContentType:='text/plain';
  realm:=ARequest.ContentFields.Values['realm'];
  cmdline:=ARequest.ContentFields.Values['cmd'];
  case realm of
    '#terminal': MainShell(cmdline, ARequest, AResponse);
    'login': LoginShell(cmdline, ARequest, AResponse);
    'vfs': AResponse.Content:=VFSShell(cmdline, ARequest.ContentFields.Values['wh']);
  else
    AResponse.Content:='INVALID REALM!';
  end;
end;

procedure TPortfolioModule.VFSRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  Handled:=True;
  AResponse.ContentType:='application/json';
  AResponse.Content:=QVFSToJSON(ARequest.QueryFields.Values['path']);
end;

procedure TPortfolioModule.VMRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  if ARequest.Method <> 'POST' then
    Exit;
  Handled:=True;
  AResponse.ContentType:='application/json';
  AResponse.Content:='{"command":"test command", "params":"Some parameters.", "enabled":true}';
end;

function TPortfolioModule.GetFile(AFile: string): string;
begin
  with TStringStream.Create do
    try
      LoadFromFile(AFile);
      Result:=DataString;
    finally
      Free;
    end;
end;

procedure TPortfolioModule.DoOpCode(op: char);
begin

end;

function TPortfolioModule.RunScript(AFile, ALoc, params: string): string;
var
  f: PVFSFile;
begin
  Result:='?SYNTAX ERROR';
  f:=GetVFSData(AFile, ALoc);
  if f = Nil then
    Exit;
  if f^.typ = ftExec then
  begin
    with TPortfolioScript.Create(Nil) do
      try
        Setup(params);
        Script:=f^.data;
        Run;
        Result:=Output;
      finally
        Free;
      end;
  end;
end;

procedure TPortfolioModule.MainShell(command: string; ARequest: TRequest;
  AResponse: TResponse);
var
  cmd: string;
begin
  cmd:=getToken(command);
  if cmd = 'ver' then
    AResponse.Content:='Portfolio Server v0.1'
  else if cmd = 'echo' then
    AResponse.Content:=getToken(command)
  else if cmd = 'help' then
    AResponse.Content:='Commands available: ver, echo'
  else
    AResponse.Content:=RunScript(cmd, 'Apps', command);
end;

procedure TPortfolioModule.LoginShell(command: string; ARequest: TRequest;
  AResponse: TResponse);
var
  cmd: string;
begin
  cmd:=getToken(command);
  case cmd of
    'ver': AResponse.Content:='Login Shell v0.1';
  else
    AResponse.Content:='Bad command or filename.';
  end;
end;

function TPortfolioModule.UserAuth(const user, pass: string): Boolean;
var
  f: PVFSFile;
begin
  Result:=False;
  f:=GetVFSData(user, 'VFSAuth');
  if f = Nil then
    Exit;
  if pass = f^.data then
    Result:=True;
end;

initialization
  RegisterHTTPModule('Portfolio', TPortfolioModule);
end.

