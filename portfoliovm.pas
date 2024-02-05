unit PortfolioVM;

{$mode objfpc}

interface

uses
  Classes, SysUtils, browserconsole, Widgets, marked, Web, ajaxlib, qvfs,
  strutils;

type

  TVMOutput = reference to procedure(s: string);

  { TPortfolioVM }

  TPortfolioVM = class(TComponent)
  private
    FData: string;
    FCP: integer;
    FRemoteFile: TWebRequest;
    function NextOp: char;
    Procedure MarkdownFail(Sender: TObject; Const aString : String);
    procedure AssignProp(aID, aProp, aValue: string);
    procedure HandleFile;
    procedure LoadFile(AFile: string);
  protected
    function GetString: string;
    procedure Output(s: string); virtual;
  public
    constructor Create(AOwner: TComponent; AData: string);
    procedure Run;
  end;

  { TSysRunner }

  TSysRunner = class(TPortfolioVM)
  private
    FOnOutput: TVMOutput;
    procedure GotDir(f: TVFSFile);
    procedure GetDir(f: TVFSFile);
    procedure VMCallback(f: TVFSFile);
  protected
    procedure Output(s: string); override;
  public
    property OnOutput: TVMOutput read FOnOutput write FOnOutput;
    procedure RunFile(AFile, ALoc: string);
  end;

implementation

{ TSysRunner }

procedure TSysRunner.GotDir(f: TVFSFile);
var
  e: TJSHTMLElement;
  icon: string;
begin
  if f.typ = -1 then
    Exit;
  case f.typ of
    0: icon:='dir';
    1: icon:='binary';
    2: icon:='layout';
    3: icon:='text';
    4: icon:='portal';
    5: icon:='folder.sec';
    6: icon:='quill';
    7: icon:='comp.blue';
    8: icon:='comp.gray';
    13: icon:='script';
  else
    icon:='generic';
  end;
  e:=TJSHTMLElement(document.getElementById('TabBody'));
  icon:='<img src="/icons/'+icon+'.png"/>';
  e.innerHTML:=e.innerHTML+'<a href="#'+f.wh+':'+f.fna+'">'+icon+f.fna+'</a><br/>';
end;

procedure TSysRunner.GetDir(f: TVFSFile);
var
  data, fna: string;
  fs: TVFSRequest;
begin
  if f.data[1] = 'V' then
  begin
    FBody.setContent('VFS Directory of '+f.fna+'<hr>');
    data:=RightStr(f.data, Length(f.data)-1);
    repeat
      fna:=Copy2SymbDel(data, ';');
      if fna <> '' then
      begin
        fs:=TVFSRequest.Create(Nil, 'get', '/Portfolio/VFS');
        fs.OnCallback:=@GotDir;
        fs.GetFile(fna, f.fna);
      end;
    until fna = '';
  end
  else
    FBody.setContent('Directory Unavailable.');
end;

procedure TSysRunner.VMCallback(f: TVFSFile);
var
  t: TMarkdownFile;
begin
  if f.typ = -1 then
    Output('File not found.')
  else if f.typ = 0 then
    GetDir(f)
  else if f.typ = 2 then
    t:=TMarkdownFile.Create(Self, f.data, 'TabBody', @MarkdownFail)
  else if f.typ = 7 then
  begin
    FData:=f.data;
    FCP:=1;
    Run;
  end
  else
    Output('Unhandled file-type.');
end;

procedure TSysRunner.Output(s: string);
begin
  if Assigned(FOnOutput) then
    FOnOutput(s)
  else
    FBody.setContent(s);
end;

procedure TSysRunner.RunFile(AFile, ALoc: string);
begin
  FileSys.OnCallback:=@VMCallback;
  FileSys.GetFile(AFile, ALoc);
end;

{ TPortfolioVM }

procedure TPortfolioVM.Output(s: string);
begin
  Writeln(s);
end;

function TPortfolioVM.NextOp: char;
begin
  Result:=FData[FCP];
  Inc(FCP);
end;

function TPortfolioVM.GetString: string;
var
  c: char;
begin
  Result:='';
  repeat
    c:=NextOp;
    if c <> '~' then
      Result:=Result+c;
  until c = '~';
end;

procedure TPortfolioVM.MarkdownFail(Sender: TObject; const aString: String);
begin
  FBody.setContent('Markdown failed to load: '+aString);
end;

procedure TPortfolioVM.AssignProp(aID, aProp, aValue: string);
begin
  TJSHTMLElement(document.getElementById(aID)).Properties[aProp]:=aValue;
end;

procedure TPortfolioVM.HandleFile;
begin
  if not FRemoteFile.Complete then
    Exit;
  Output(FRemoteFile.responseText);
  FreeAndNil(FRemoteFile);
end;

procedure TPortfolioVM.LoadFile(AFile: string);
begin
  if Assigned(FRemoteFile) then
  begin
    Output('File Load Request already in progress...');
    Exit;
  end;
  FRemoteFile:=TWebRequest.Create(Nil, 'get', AFile);
  FRemoteFile.OnChange:=@HandleFile;
  FRemoteFile.DoRequest;
end;

constructor TPortfolioVM.Create(AOwner: TComponent; AData: string);
begin
  inherited Create(AOwner);
  FData:=AData;
  FCP:=1;
end;

procedure TPortfolioVM.Run;
var
  op: char;
  running: boolean;
  t: TMarkdownFile;
  target: string;
begin
  running:=True;
  repeat
    op:=NextOp;
    case op of
      '$': target:='TabBody';
      'A': window.alert(GetString);
      'a': AssignProp(GetString, GetString, GetString);
      'h': TJSHTMLElement(document.getElementById(GetString)).hidden:=True;
      's': TJSHTMLElement(document.getElementById(GetString)).hidden:=False;
      'C': TJSHTMLElement(document.getElementById(GetString)).classList.add(GetString);
      'c': TJSHTMLElement(document.getElementById(GetString)).classList.remove(GetString);
      'P': Output(GetString);
      'L': LoadFile(GetString);
      'p': TJSHTMLElement(document.getElementById(target)).innerHTML:=GetString;
      'B': LoadFileInto(GetString, FBody);
      'T': target:=GetString;
      'M': t:=TMarkdownFile.Create(Self, GetString, target, @MarkdownFail);
      'X': running:=False;
    else
      running:=False;
      if op = '#' then
        Output('Program can only run on server.')
      else
        Output('Invalid Op Code at '+IntToStr(FCP-1));
    end;
  until not running;
end;

end.

