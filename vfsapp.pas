unit VFSApp;

{$mode objfpc}

interface

uses
  Classes, SysUtils, libjquery, jsterm, WebTerm, Widgets, jsonlib, JS, qvfs;

type

  { TVFSApp }

  TVFSApp = class(TObject)
  private
    FTerm: TJQuery;
    FOnLogin: TLoginEvent;
    FVFSDir: string;
    procedure VFSAuth(data: TJSONData);
    procedure VFSLogin(user, password: string; callback: TLoginEvent);
    procedure VFSCommand(command: string; term: TJQuery);
    procedure ProcessCommand;
    procedure VFSCallback(f: TVFSFile);
    procedure VFSAppStart(term: TJQuery);
  public
    constructor Create;
  end;

procedure StartVFSTerm;

implementation

const
  VFS_HOME = 'RootFS';

var
  TermApp: TVFSApp;

procedure StartVFSTerm;
begin
  if Assigned(TermApp) then
    TermApp.Free;
  TermApp:=TVFSApp.Create;
end;

{ TVFSApp }

procedure TVFSApp.VFSAuth(data: TJSONData);
begin
  FOnLogin(data.Strings['token']);
  FTerm.Enabled:=True;
end;

procedure TVFSApp.VFSLogin(user, password: string; callback: TLoginEvent);
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
    Add('realm=vfs');
  end;
  AuthSys.DoRequest(data);
  data.Free;
end;

procedure TVFSApp.VFSCommand(command: string; term: TJQuery);
var
  cmd: string;
  data: TStringList;
begin
  cmd:=getToken(command);
  if cmd = 'cd' then
  begin
    FileSys.OnCallback:=@VFSCallback;
    FileSys.GetFile(getToken(command), FVFSDir);
    term.Enabled:=False;
  end
  else if cmd = 'home' then
  begin
    FVFSDir:=VFS_HOME;
    term.Prompt:=FVFSDir+'/>';
  end
  else if cmd = 'open' then
    GetVFSFile(getToken(command), FVFSDir, FBody)
  else
  begin
    FTerm.Enabled:=False;
    data:=TStringList.Create;
    with data do
    begin
      Add('cmd='+encodeURIComponent(cmd+' '+command));
      Add('realm='+encodeURIComponent(FTerm.Name));
      Add('wh='+encodeURIComponent(FVFSDir));
    end;
    CmdSys.DoRequest(data);
    data.Free;
  end;
end;

procedure TVFSApp.ProcessCommand;
begin
  if not CmdSys.Complete then
    Exit;
  FTerm.Echo(CmdSys.responseText);
  FTerm.Enabled:=True;
end;

procedure TVFSApp.VFSCallback(f: TVFSFile);
begin
  if f.typ = 0 then
    FVFSDir:=f.fna;
  FTerm.Prompt:=FVFSDir+'/>';
  FTerm.Enabled:=True;
end;

procedure TVFSApp.VFSAppStart(term: TJQuery);
var
  params: TTerminalState;
begin
  FTerm:=term;
  CmdSys.OnChange:=@ProcessCommand;
  AuthSys.OnJSON:=@VFSAuth;
  params:=LoginState('vfs', FVFSDir+'/>', @VFSLogin);
  FTerm.Echo('VFS Shell Login');
  term.Push(@VFSCommand, params);
end;

constructor TVFSApp.Create;
begin
  FVFSDir:=VFS_HOME;
  StartWebTerm;
  StartTermApp(@VFSAppStart);
end;

end.

