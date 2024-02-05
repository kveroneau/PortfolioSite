unit PortfolioScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ScriptSystem;

type

  { TPortfolioScript }

  TPortfolioScript = class(TScriptSystem)
  private
    FOutput: string;
    FInput: string;
    procedure CaptureOutput(data: string);
    function ProvideInput: string;
    procedure ProcessOp(op: char);
  public
    property Output: string read FOutput;
    procedure Setup(params: string);
  end;

implementation

{ TPortfolioScript }

procedure TPortfolioScript.CaptureOutput(data: string);
begin
  FOutput:=FOutput+data+#10;
end;

function TPortfolioScript.ProvideInput: string;
begin
  Result:='';
end;

procedure TPortfolioScript.ProcessOp(op: char);
begin
  if op = '$' then
  begin
    Running:=False;
    CaptureOutput('This program is meant to run in the browser.');
  end;
end;

procedure TPortfolioScript.Setup(params: string);
begin
  FOutput:='';
  OnOutput:=@CaptureOutput;
  OnInput:=@ProvideInput;
  OnOpCode:=@ProcessOp;
  FInput:=params;
end;

end.

