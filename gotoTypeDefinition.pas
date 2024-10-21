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
// <https://www.gnu.org/licenses/>

// This file was adapted by github.com/@socialmediarefugee for inclusion in the Pascal Language Server

/// Pasdoc
/// @author @socialmediarefugee
/// 

unit gotoTypeDefinition;

{$mode objfpc}{$H+}

interface

uses
  Classes, 
  URIParser, 
  CodeToolManager, 
  CodeCache, 
  BasicCodeTools,
  lsp, 
  basic;

type
  
  { TGotoImplementation }
  
  TGotoTypeDefinition = class(specialize TLSPRequest<TTextDocumentPositionParams, TLocation>)
    function Process(var Params: TTextDocumentPositionParams): TLocation; override;
  end;

implementation
uses
  diagnostics;
  
function TGotoTypeDefinition.Process(var Params: TTextDocumentPositionParams): TLocation;
var
  fileName: String;
  Code: TCodeBuffer;
  NewCode: TCodeBuffer;
  X, Y: Integer;
  NewX, NewY, NewTopLine, BlockTopLine, BlockBottomLine: integer;
begin with Params do
  begin
    UriToFilename(textDocument.uri,fileName);
    
    Code := CodeToolBoss.FindFile(fileName);
    X := position.character;
    Y := position.line;

    if CodeToolBoss.FindDeclaration(Code, X + 1, Y + 1, NewCode, NewX, NewY, NewTopLine, BlockTopLine, BlockBottomLine) then
      begin
        Result := TLocation.Create;
        Result.URI := PathToURI(NewCode.Filename);
        Result.Range := TRange.Create(NewY - 1, NewX - 1);
      end
    else
      begin
        Result := nil;
        PublishDiagnostic;
      end;
  end;
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/typedefinition', TGotoTypeDefinition);
end.