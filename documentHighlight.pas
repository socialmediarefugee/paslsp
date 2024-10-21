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

unit documentHighlight;

{$mode objfpc}{$H+}
{$scopedenums on}

interface

uses
  Classes, 
  URIParser, CodeToolManager, CodeCache, IdentCompletionTool, BasicCodeTools, CodeTree,
  lsp, basic;

type
  TDocumentHighlightKind = (
      __UNUSED__ = 0,
      // A textual occurrence.
      Text = 1,
      // Read-access of a symbol, like reading a variable.
      Read = 2,
      // Write-access of a symbol, like writing to a variable.
      Write = 3
    );

  { TDocumentHighlight }
  TDocumentHighlight = class(TCollectionItem)
  private
    fRange: TRange;
    fKind: TDocumentHighlightKind;
  published
    // The range this highlight applies to.
    property range: TRange read fRange write fRange;

    // The highlight kind, default is DocumentHighlightKind.Text.
    property kind: TDocumentHighlightKind read fKind write fKind;
  end;

  TDocumentHighlightItems = specialize TGenericCollection<TDocumentHighlight>;

  { DocumentHighlightParams }

  TDocumentHighlightParams = class(TTextDocumentPositionParams)
  end;

  { TDocumentHighlightRequest }

  TDocumentHighlightRequest = class(specialize TLSPRequest<TDocumentHighlightParams, TDocumentHighlightItems>)
    function Process(var Params: TDocumentHighlightParams): TDocumentHighlightItems; override;
  end;

implementation

function TDocumentHighlightRequest.Process(var Params: TDocumentHighlightParams): TDocumentHighlightItems;
var
  Code: TCodeBuffer;
  X, Y: Integer;
  NewCode: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
  Item: TDocumentHighlight;
  Identifier: String;
begin with Params do
  begin
    Code := CodeToolBoss.FindFile(UriToFilenameEx(textDocument.uri));
    X := position.character;
    Y := position.line;
    Result := nil;
    
    if CodeToolBoss.FindBlockCounterPart(Code, X + 1, Y + 1, NewCode, NewX, NewY, NewTopLine) then
      begin
        if Result = nil then
          Result := TDocumentHighlightItems.Create;
        Item := TDocumentHighlight(Result.Add);
        Item.kind := TDocumentHighlightKind.Text;
        Item.range := TRange.Create(NewY - 1, NewX - 1, Y, X);
      end;
  end;
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/documentHighlight', TDocumentHighlightRequest);
end.