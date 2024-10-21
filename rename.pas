// Pascal Language Server
// Copyright 2020 Ryan Joseph

// This file is part of Pascal Language Server.

// Pascal Language Server is free software: you can redistribute it
// and/or modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.

// Pascal Language Server is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the implied warranty
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Pascal Language Server.  If not, see
// <https://www.gnu.org/licenses/>.

unit rename;

{$mode objfpc}{$H+}

interface

uses
  Classes, URIParser, CodeToolManager, CodeCache, BasicCodeTools,
  lsp, basic,fpjson,fpjsonrpc,avl_tree, CTUnitGraph ,settings,
  { LazUtils }
  LazFileUtils;

type



  { TRename }
  

  TRename = class(specialize TLSPRequest<TRenameParams, TWorkspaceEdit>)
    function DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData; override;
    
    //function Process(var Params: TRenameParams): TWorkspaceEdit; override;
  end;

implementation
uses
  diagnostics;

function FindReferences(Filename, MainFilename: String; X, Y: Integer;out Identifier:string):TAVLTree;
var
  DeclCode, StartSrcCode, Code: TCodeBuffer;
  ListOfPCodeXYPosition: TFPList;
  DeclX, DeclY, DeclTopLine, i: Integer;
  CurLine: string;
  Graph: TUsesGraph;
  Cache: TFindIdentifierReferenceCache;
  ANode, Node: TAVLTreeNode;
  CodePos: PCodeXYPosition;
  Files: TStringList;
  Completed: boolean;
  UGUnit: TUGUnit;
begin

  // Step 1: load the file
  StartSrcCode:=CodeToolBoss.LoadFile(Filename,false,false);

  // Step 2: find the main declaration
  if not CodeToolBoss.FindMainDeclaration(StartSrcCode,
    X,Y,
    DeclCode,DeclX,DeclY,DeclTopLine) then
  begin
    //PublishDiagnostic('FindMainDeclaration failed in '+StartSrcCode.FileName+' at '+IntToStr(Y)+':'+IntToStr(X));
    ExitCode:=-1;
    exit;
  end;

  // Step 3: get identifier
  CodeToolBoss.GetIdentifierAt(DeclCode,DeclX,DeclY,Identifier);
  writeln(StdErr, 'Found identifier: ',Identifier);

  // Step 4: collect all modules of program
  Files:=TStringList.Create;
  ListOfPCodeXYPosition:=nil;
  Result:=nil;
  Cache:=nil;
  try
    Files.Add(DeclCode.Filename);
    if CompareFilenames(DeclCode.Filename,StartSrcCode.Filename)<>0 then
      Files.Add(DeclCode.Filename);

    // parse all used units
    Graph:=CodeToolBoss.CreateUsesGraph;
    try
      Graph.AddStartUnit(MainFilename);
      Graph.AddTargetUnit(DeclCode.Filename);
      Graph.Parse(true,Completed);
      Node:=Graph.FilesTree.FindLowest;
      while Node<>nil do begin
        UGUnit:=TUGUnit(Node.Data);
        Files.Add(UGUnit.Filename);
        Node:=Node.Successor;
      end;
    finally
      Graph.Free;
    end;

    // Step 5: find references in all files
    for i:=0 to Files.Count-1 do begin
      writeln(Stderr, 'Searching ', Files[i], '...');
      Code:=CodeToolBoss.LoadFile(Files[i],true,false);
      if Code=nil then begin
        writeln(stderr, 'unable to load "',Files[i],'"');
        continue;
      end;
      // search references
      CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
      if not CodeToolBoss.FindReferences(
        DeclCode,DeclX,DeclY,
        Code, true, ListOfPCodeXYPosition, Cache) then
      begin
        PublishDiagnostic('FindReferences failed in "'+Code.Filename+'"');
        continue;
      end;
      if ListOfPCodeXYPosition=nil then continue;
      // In order to show all references after any parser error, they are
      // collected in a tree
      if Result=nil then
        Result:=CodeToolBoss.CreateTreeOfPCodeXYPosition;
      CodeToolBoss.AddListToTreeOfPCodeXYPosition(ListOfPCodeXYPosition,
                                              Result,true,false);
    end;


  finally
    Files.Free;
    CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
    Cache.Free;
    Flush(stderr);
  end;
end;



//todo: use documentChanges to support needsConfirmation
function TRename.DoExecute(const Params: TJSONData; 
  AContext: TJSONRPCCallContext): TJSONData;
var
  Input: TRenameParams;
  Path,Identifier: String;
  we:TWorkspaceEdit;
  edit:TTextEdit;
  items:TChangeItems;
  X, Y: Integer;
  av_tree:TAVLTree;
  code:TCodeBuffer;
  pos:TPoint;
  ANode, Node: TAVLTreeNode;
  CodePos: PCodeXYPosition;

begin
  Input := specialize TLSPStreaming<TRenameParams>.ToObject(Params);
  we:=TWorkspaceEdit.Create;
  


  try
    Path := UriToFilenameEx(input.textDocument.uri);
    X := input.position.character;
    Y := input.position.line;

    Code := CodeToolBoss.FindFile(Path);
   
    // if the main program file was provided via initializationOptions -> program
    // then use this unit as the root for searching, otherwise default to the
    // current text document
    if ServerSettings.&program <> '' then
      av_tree:=FindReferences(Path, ServerSettings.&program, X + 1, Y + 1,identifier)
    else
      av_tree:=FindReferences(Path, Path, X + 1, Y + 1,identifier);

    if not assigned(av_tree) then exit;

      ANode:=av_tree.FindHighest;
      while ANode<>nil do begin
        CodePos:=PCodeXYPosition(ANode.Data);
        path:=PathToURI(CodePos^.Code.Filename);
        if not we.changes.TryGetData(path,items) then
        begin
          items:=TChangeItems.Create;
          we.changes.Add(path,items);
        end;

        edit:=items.Add;
        edit.range.start.line:=CodePos^.Y - 1;
        edit.range.start.character:=CodePos^.X - 1;
        edit.range.&end.line:=CodePos^.Y - 1;
        edit.range.&end.character:=CodePos^.X-1 +length(identifier);
        edit.newText:=input.newName;
        //writeln(StdErr, '  Found: ', CodePos^.Code.Filename, ' @ ', CodePos^.Y, ',',CodePos^.X);
        ANode:=av_tree.FindPrecessor(ANode);
      end;
  finally
    Result:=we.ToJson();
    we.Free;
    Input.Free;  
  end;
  



end;

// function TRename.Process(var Params: TRenameParams): TWorkspaceEdit;
// var edit:TTextEdit;
// begin
//   edit:=TTextEdit.Create(Nil);
//   result:=TWorkspaceEdit.Create;
//   edit.range:=TRange.Create(Params.position.line,Params.position.character);
//   edit.newText:='test';
  
//   Result.changes:= TChangeDictory.Create;

//   Result.changes.Add(Params.textDocument.uri,edit);
//   edit:=Result.changes.values.ToArray[0];

// end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/rename', TRename);
end.
