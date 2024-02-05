unit Widgets;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  Classes, SysUtils, Web, bulma, ajaxlib, browserconsole, timer, webrouter,
  jsonlib, JS, marked, qvfs;

procedure InitWidgets;
function ShowWelcome(aEvent: TJSMouseEvent): Boolean;
function AboutMe(aEvent: TJSMouseEvent): Boolean;
function AboutSite(aEvent: TJSMouseEvent): Boolean;
function ContactMe(aEvent: TJSMouseEvent): Boolean;
function ShowProjects(aEvent: TJSMouseEvent): Boolean;
function ShowSkills(aEvent: TJSMouseEvent): Boolean;
function ShowJobs(aEvent: TJSMouseEvent): Boolean;
function ShowMyDrive(aEvent: TJSMouseEvent): Boolean;
procedure GetVFSFile(AFile, ALoc: string; target: TBulmaWidget);
procedure LoadFileInto(AFile: string; target: TBulmaWidget);
procedure LoadMDInto(AFile: string; target: string);

var
  FBody: TBulmaWidget;
  AuthSys: TJSONRequest;
  CmdSys: TWebRequest;
  FileSys: TVFSRequest;

implementation

const
  SUBTITLE = 'All this content is going to be updated before I upload.';

var
  FRemoteFile: TWebRequest;
  FTarget: TBulmaWidget;
  FTimer: TTimer;
  FFullName: TBulmaInput;
  FEmail: TBulmaInput;
  FReason: TBulmaInput;

procedure HandleFile;
begin
  if not FRemoteFile.Complete then
    Exit;
  FTarget.setContent(FRemoteFile.responseText);
  FreeAndNil(FRemoteFile);
end;

procedure LoadFileInto(AFile: string; target: TBulmaWidget);
begin
  if Assigned(FRemoteFile) then
  begin
    target.setContent('<pre>Load already in progress...</pre>');
    Exit;
  end;
  WriteLn('Loading from: '+AFile);
  FTarget:=target;
  FRemoteFile:=TWebRequest.Create(Nil, 'get', AFile);
  FRemoteFile.OnChange:=@HandleFile;
  FRemoteFile.DoRequest;
end;

procedure MarkdownLoadFail(Sender: TObject; data: string);
begin
  window.alert(data);
end;

procedure LoadMDInto(AFile: string; target: string);
var
  t: TMarkdownFile;
begin
  t:=TMarkdownFile.Create(Nil, AFile, target, Nil);
end;

procedure VFSAction(f: TVFSFile);
begin
  if f.typ = -1 then
    FTarget.setContent('<b>FileSys Error:</b> '+f.data)
  else
    FTarget.setContent(f.data);
end;

procedure InitWidgets;
begin
  FBody:=TBulmaWidget.Create(Nil, 'TabBody');
  AuthSys:=TJSONRequest.Create(Nil, 'post', '/Portfolio/Auth');
  CmdSys:=TWebRequest.Create(Nil, 'post', '/Portfolio/Command');
  FileSys:=TVFSRequest.Create(Nil, 'get', '/Portfolio/VFS');
end;

function ExploreClick(aEvent: TJSMouseEvent): Boolean;
begin
  FBody.setContent('<pre>Thank you for exploring with me today, please wait a moment while I prepare some stuff...</pre>');
  Router.Push('/explore');
end;

function ShowWelcome(aEvent: TJSMouseEvent): Boolean;
var
  buf: string;
  btn: TBulmaButton;
begin
  buf:='<h1 class="title">Kevin Veroneau''s Online Portfolio</h1>';
  buf:=buf+'<p class="subtitle">'+SUBTITLE+'</p>';
  btn:=TBulmaButton.Create(Nil, 'Explore what I have to offer today!', 'ExploreBtn', @ExploreClick);
  btn.ButtonStyle:='is-link';
  FBody.setContent(buf+btn.renderHTML);
  btn.Bind;
end;

function AboutMe(aEvent: TJSMouseEvent): Boolean;
begin
  LoadMDInto('aboutme.md', 'TabBody');
end;

function AboutSite(aEvent: TJSMouseEvent): Boolean;
begin
  {LoadFileInto('aboutsite.html', FBody);}
  LoadMDInto('aboutsite.md', 'TabBody');
end;

function ContactSubmit(aEvent: TJSMouseEvent): Boolean;
var
  eidx: integer;
begin
  eidx:=FEmail.JSValue.indexOf('@');
  if (eidx < 1) or (FEmail.JSValue.indexOf('.', eidx) < 1) then
    Exit;
  window.alert('Thank you, '+FFullName.Value+' for contacting me.');
end;

function ContactMe(aEvent: TJSMouseEvent): Boolean;
var
  frm: TBulmaForm;
begin
  frm:=TBulmaForm.Create(Nil);
  FFullName:=frm.AddInput('Your Name:', 'YourName');
  FEmail:=frm.AddInput('Your Email:', 'email');
  FEmail.ControlType:=ctEmail;
  FReason:=frm.AddInput('Reason for Contact:', 'reason');
  frm.SubmitText:='Contact Kevin';
  frm.OnSubmit:=@ContactSubmit;
  FBody.setContent(frm.renderHTML);
  frm.Bind;
end;

function ShowProjects(aEvent: TJSMouseEvent): Boolean;
begin
  Router.Push('/Portfolio:Projects');
end;

function ShowSkills(aEvent: TJSMouseEvent): Boolean;
begin
  Router.Push('/Portfolio:/Skills');
end;

function ShowJobs(aEvent: TJSMouseEvent): Boolean;
begin
  Router.Push('/Portfolio:/Jobs');
end;

function ShowMyDrive(aEvent: TJSMouseEvent): Boolean;
begin
  Router.Push('/Portfolio:MyDrive');
end;

procedure GetVFSFile(AFile, ALoc: string; target: TBulmaWidget);
begin
  FTarget:=target;
  FileSys.OnCallback:=@VFSAction;
  FileSys.GetFile(AFile, ALoc);
end;

end.

