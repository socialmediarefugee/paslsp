unit CodeToolsUtil;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FileProcs, LazUtils,LCLProc,
  // Codetools
  Laz_AVL_Tree, ExprEval,DefineTemplates,CodeToolManager,CodeCache,LinkScanner,sourcelog,
  BasicCodeTools,
  //pasls
  inactiveRegions;

type
  TCodeToolsDefinesNodeValues = class
  public
    Node: TDefineTemplate;
    ValueParsed: boolean;
    ParsedValue: string;
    ExpressionCalculated: boolean;
    ExpressionResult: string;
    Execute: boolean;
  end;

  { TCodeToolsUtil }

  TCodeToolsUtil = class
  private
    //FDefineTree: TDefineTree;
    FNodeValues: TAvlTree;
    fReport: TStringList;

    procedure DefineTreeCalculate({%H-}DefineTree: TDefineTree; Node: TDefineTemplate;
                  ValueParsed: boolean; const ParsedValue: string;
                  ExpressionCalculated: boolean; const ExpressionResult: string;
                  Execute: boolean);

  protected

  public

    constructor Create;
    destructor Destroy;override;
    procedure AddCustomMacro;
    procedure WriteDefinesDebugReport;
    procedure WriteUnitDirectives(Code: TCodeBuffer);

    procedure CheckInactiveRegion(Code:TCodeBuffer;uri:String);
  published
  end;

  function CompareNodeValues(Data1, Data2: Pointer): Integer;
  function CompareNodeAndNodeValues(Node, NodeValues: Pointer): Integer;
var
  CodeUtilBoss:TCodeToolsUtil;
implementation

function CompareNodeValues(Data1, Data2: Pointer): Integer;
begin
  Result:=ComparePointers(TCodeToolsDefinesNodeValues(Data1).Node,
                          TCodeToolsDefinesNodeValues(Data2).Node);
end;

function CompareNodeAndNodeValues(Node, NodeValues: Pointer): Integer;
begin
  Result:=ComparePointers(TCodeToolsDefinesNodeValues(Node),
                          TCodeToolsDefinesNodeValues(NodeValues).Node);
end;

{ TCodeToolsUtil }

procedure TCodeToolsUtil.DefineTreeCalculate(DefineTree: TDefineTree;
  Node: TDefineTemplate; ValueParsed: boolean; const ParsedValue: string;
  ExpressionCalculated: boolean; const ExpressionResult: string;
  Execute: boolean);
var
  NewNodeValues: TCodeToolsDefinesNodeValues;
begin
  NewNodeValues:=TCodeToolsDefinesNodeValues.Create;
  NewNodeValues.Node:=Node;
  NewNodeValues.ValueParsed:=ValueParsed;
  NewNodeValues.ParsedValue:=ParsedValue;
  NewNodeValues.ExpressionCalculated:=ExpressionCalculated;
  NewNodeValues.ExpressionResult:=ExpressionResult;
  NewNodeValues.Execute:=Execute;
  if FNodeValues=nil then
    FNodeValues:=TAvlTree.Create(@CompareNodeValues);
  FNodeValues.Add(NewNodeValues);
end;

constructor TCodeToolsUtil.Create;
begin
   inherited Create;
end;

destructor TCodeToolsUtil.Destroy;
begin
  inherited Destroy;
end;

procedure TCodeToolsUtil.AddCustomMacro;
begin
end;

procedure TCodeToolsUtil.WriteDefinesDebugReport;
// let the codetools calculate the defines for the directory

  procedure AddNodeReport(Prefix: string; DefTempl: TDefineTemplate);
  var
    AVLNode: TAvlTreeNode;
    NodeValues: TCodeToolsDefinesNodeValues;
    s: string;
  begin
    while DefTempl <> nil do
    begin
      s := Prefix + 'Name="' + DefTempl.Name + '"';
      s := s + ' Description="' + DefTempl.Description + '"';
      s := s + ' Action="' + DefineActionNames[DefTempl.Action] + '"';
      s := s + ' Variable="' + DefTempl.Variable + '"';
      s := s + ' Value="' + dbgstr(DefTempl.Value) + '"';
      if FNodeValues <> nil then
      begin
        AVLNode := FNodeValues.FindKey(DefTempl, @CompareNodeAndNodeValues);
        if AVLNode <> nil then
        begin
          NodeValues := TCodeToolsDefinesNodeValues(AVLNode.Data);
          if NodeValues.ValueParsed then
            s := s + ' ParsedValue="' + dbgstr(NodeValues.ParsedValue) + '"';
          if NodeValues.ExpressionCalculated then
            s := s + ' ExpressionResult="' + dbgstr(NodeValues.ExpressionResult) + '"';
          s := s + ' Executed="' + dbgs(NodeValues.Execute) + '"';
        end;
      end;
      fReport.Add(s);
      if DefTempl.FirstChild <> nil then
        AddNodeReport(Prefix + '    ', DefTempl.FirstChild);
      DefTempl := DefTempl.Next;
    end;
  end;

var
  Dir: string;
  Defines: TExpressionEvaluator;
  i: integer;

  Value: string;
  OldOnCalculate: TDefTreeCalculate;
  DefineTree: TDefineTree;

begin
  Dir := ExpandFileName(GetCurrentDir);
  DefineTree := CodeToolBoss.DefineTree;
  DefineTree.ClearCache;// make sure the defines are reparsed
  OldOnCalculate := DefineTree.OnCalculate;

  DefineTree.OnCalculate := @DefineTreeCalculate;
  try
    Defines := DefineTree.GetDefinesForDirectory(Dir, False);
  finally
    DefineTree.OnCalculate := OldOnCalculate;
  end;

  fReport := TStringList.Create;
  try
    fReport.Add('Directory: ' + Dir);
    if Defines <> nil then
    begin
      fReport.Add('Defines:');
      for i := 0 to Defines.Count - 1 do
      begin

        fReport.Add(Defines.Names(i) + '=' + dbgstr(Defines.Values(i)));
      end;
      fReport.Add('');
    end
    else
    begin
    end;

    // add all nodes to report
    fReport.Add('Tree:');
    AddNodeReport('  ', DefineTree.RootTemplate);
  finally
    DebugLn(fReport.Text);
    FreeAndNil(fReport);
  end;
end;

procedure TCodeToolsUtil.WriteUnitDirectives(Code: TCodeBuffer);
var
  Scanner: TLinkScanner;

  i: Integer;
  Dir: PLSDirective;
  FirstSortedIndex: integer;
  LastSortedIndex: integer;
begin
  if Code=nil then
    exit;


  // parse the unit
  if not CodeToolBoss.ExploreUnitDirectives(Code,Scanner) then
    raise Exception.Create('parser error');
  scanner.WriteDebugReport;
  DebugLn('-----------------------------------------------');
  DebugLn(Scanner.CleanedSrc);
  DebugLn('-----------------------------------------------');
  writeln('Directives in compile order:');
  for i:=0 to Scanner.DirectiveCount-1 do begin
    Dir:=Scanner.Directives[i];
    DebugLn([i,'/',Scanner.DirectiveCount,
      ' CleanPos=',Dir^.CleanPos,'=',Scanner.CleanedPosToStr(Dir^.CleanPos),
      ' Level=',Dir^.Level,' ',dbgs(Dir^.State),
      ' "',ExtractCommentContent(Scanner.CleanedSrc,Dir^.CleanPos,Scanner.NestedComments),'"']
      );
  end;
  DebugLn('-----------------------------------------------');
  DebugLn('Directives sorted for Code and SrcPos:');
  for i:=0 to Scanner.DirectiveCount-1 do begin
    Dir:=Scanner.DirectivesSorted[i];
    DebugLn([i,'/',Scanner.DirectiveCount,
      ' CleanPos=',Dir^.CleanPos,'=',Scanner.CleanedPosToStr(Dir^.CleanPos),
      ' Level=',Dir^.Level,' ',dbgs(Dir^.State),
      ' "',ExtractCommentContent(Scanner.CleanedSrc,Dir^.CleanPos,Scanner.NestedComments),'"' ]
      );
    if Scanner.FindDirective(Code,Dir^.SrcPos,FirstSortedIndex,LastSortedIndex)
    then begin
      if FirstSortedIndex<LastSortedIndex then
        DebugLn([' MULTIPLE: ',FirstSortedIndex,'-',LastSortedIndex]);
    end else begin
      raise Exception.Create('inconsistency: Scanner.FindDirective failed');
    end;
    DebugLn;
  end;

  DebugLn('-----------------------------------------------');
end;
procedure TCodeToolsUtil.CheckInactiveRegion(Code:TCodeBuffer;uri:String);
var   
  notify:TInactiveRegionsNotification;
  regions:TRegionsItems;
  input:TInputRegion;
  Scanner: TLinkScanner;
  i: Integer;
  Dir: PLSDirective;
  isAdd:Boolean;
  cursorPos:Integer;
  acode:Pointer;
  line,col:Integer;

begin
  if Code=nil then
    exit;
  
  // parse the unit
  if not CodeToolBoss.ExploreUnitDirectives(Code,Scanner) then
     exit;
  isAdd:=False;
  regions:=TRegionsItems.Create;
  try
    for i:=0 to Scanner.DirectiveCount-1 do begin
      Dir:=Scanner.Directives[i];
    
      DebugLn([i,'/',Scanner.DirectiveCount,
        ' CleanPos=',Dir^.CleanPos,'=',Scanner.CleanedPosToStr(Dir^.CleanPos),
        ' Level=',Dir^.Level,' ',dbgs(Dir^.State),
        ' "',ExtractCommentContent(Scanner.CleanedSrc,Dir^.CleanPos,Scanner.NestedComments),'"']
        );
      if Dir^.State=lsdsInactive then
      begin
        if not isAdd then
        begin
          input:=TInputRegion(regions.Add);
          Scanner.CleanedPosToCursor(Dir^.CleanPos,cursorPos,acode);
          TSourceLog(acode).AbsoluteToLineCol(cursorPos,line,col);
          input.startline:=line;
          isAdd:=True;
        end;
      end
      else if Dir^.State=lsdsActive then
      begin
        if isAdd then
        begin
          Scanner.CleanedPosToCursor(Dir^.CleanPos,cursorPos,acode);
          TSourceLog(acode).AbsoluteToLineCol(cursorPos,line,col);
          input.endline:=line-1;
          isAdd:=False;  
        end;
      end;
    end;
    if input.endline=0 then
    begin
      input.endline:=999999;
    end;
    
    notify := TInactiveRegionsNotification.Create(uri,regions);
    notify.Send;
    notify.Free;  
    
  finally
    regions.Free;
  end;
end;
initialization
CodeUtilBoss:=TCodeToolsUtil.Create;
finalization
FreeAndNil(CodeUtilBoss);
end.
