// Pascal Language Server
// Copyright 2020 Arjan Adriaanse

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

unit basic;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$scopedenums on}

interface

uses
  Classes, SysUtils,fgl,fpjson;

type

  { TOptional }

  generic TOptional<T> = class
  private
    fHasValue: Boolean;
    fValue: T;
    function GetValue: T;
    procedure SetValue(AValue: T);
  public
    property HasValue: Boolean read fHasValue;
    property Value: T read GetValue write SetValue;
    procedure Clear;
  end;

  { TOptionalVariantBase }

  TOptionalVariantBase = class(specialize TOptional<Variant>);

  { TOptionalVariant }

  generic TOptionalVariant<T> = class(TOptionalVariantBase)
  private
    function GetValue: T;
    procedure SetValue(AValue: T);
  public
    constructor Create; overload;
    constructor Create(AValue: T); overload;
    property Value: T read GetValue write SetValue;
  end;

  { TOptionalObjectBase }

  TOptionalObjectBase = class(specialize TOptional<TObject>)
  public
    function ValueClass: TClass; virtual; abstract;
  end;

  { TOptionalObject }

  generic TOptionalObject<T> = class(TOptionalObjectBase)
  private
    function GetValue: T;
    procedure SetValue(AValue: T);
  public
    constructor Create;
    constructor Create(AValue: T);
    function ValueClass: TClass; override;
    property Value: T read GetValue write SetValue;
  end;


  TOptionalBoolean = specialize TOptionalVariant<Boolean>;
  TOptionalString = specialize TOptionalVariant<String>;
  TOptionalInteger = specialize TOptionalVariant<Integer>;
  TOptionalNumber = TOptionalInteger;

  { TGenericCollection }

  generic TGenericCollection<T> = class(TCollection)
    constructor Create;
  end;

  { TDocumentUri }

  TDocumentUri = string;

  { TPosition }

  TPosition = class(TPersistent)
  private
    fLine: Integer;
    fCharacter: Integer;
  published
    // Line position in a document (zero-based).
    property line: Integer read fLine write fLine;
    // Character offset on a line in a document (zero-based). Assuming
    // that the line is represented as a string, the `character` value
    // represents the gap between the `character` and `character + 1`.
    //
    // If the character value is greater than the line length it
    // defaults back to the line length.
    property character: Integer read fCharacter write fCharacter;
  public
    constructor Create(l, c: integer);
  end;

  { TRange }

  TRange = class(TPersistent)
  private
    fStart: TPosition;
    fEnd: TPosition;
  published
    // The range's start position.
    property start: TPosition read fStart write fStart;
    // The range's end position.
    property &end: TPosition read fEnd write fEnd;
  public
    destructor Destroy; override;
    constructor Create(line, column: integer); overload;
    constructor Create(line, column, len: integer); overload;
    constructor Create(startLine, startColumn: integer; endLine, endColumn: integer); overload;
  end;

  { TLocation }

  TLocation = class(TPersistent)
  private
    fUri: TDocumentUri;
    fRange: TRange;
  published
    property uri: TDocumentUri read fUri write fUri;
    property range: TRange read fRange write fRange;
  public
    constructor Create(Path: String; Line, Column, EndLine,EndCol: Integer); overload;
  end;

  { TLocation }

  TLocationItem = class(TCollectionItem)
  private
    fUri: TDocumentUri;
    fRange: TRange;
  published
    property uri: TDocumentUri read fUri write fUri;
    property range: TRange read fRange write fRange;
  end;
  
  TLocationItems = specialize TGenericCollection<TLocationItem>;

  { TLocationLink }

  TLocationLink = class(TPersistent)
  private
    fOriginSelectionRange: TRange;
    fTargetUri: TDocumentUri;
    fTargetRange: TRange;
    fTargetSelectionRange: TRange;
  published
    // Span of the origin of this link.
    //
    // Used as the underlined span for mouse interaction. Defaults to
    // the word range at the mouse position.
    property originSelectionRange: TRange read fOriginSelectionRange write fOriginSelectionRange;
    // The target resource identifier of this link.
    property targetUri: TDocumentUri read fTargetUri write fTargetUri;
    // The full target range of this link. If the target for example
    // is a symbol then target range is the range enclosing this
    // symbol not including leading/trailing whitespace but everything
    // else like comments. This information is typically used to
    // highlight the range in the editor.
    property targetRange: TRange read fTargetRange write fTargetRange;
    // The range that should be selected and revealed when this link
    // is being followed, e.g the name of a function.  Must be
    // contained by the the `targetRange`. See also
    // `DocumentSymbol#range`
    property targetSelectionRange: TRange read fTargetSelectionRange write fTargetSelectionRange;
  end;

  { TTextDocumentIdentifier }

  TTextDocumentIdentifier = class(TPersistent)
  private
    fUri: TDocumentUri;
  published
    property uri: TDocumentUri read fUri write fUri;
  end;

  { TVersionedTextDocumentIdentifier }

  TVersionedTextDocumentIdentifier = class(TTextDocumentIdentifier)
  private
    fVersion: string;
  published
    // The version number of this document. If a versioned text
    // document identifier is sent from the server to the client and
    // the file is not open in the editor (the server has not received
    // an open notification before) the server can send `null` to
    // indicate that the version is known and the content on disk is
    // the master (as speced with document content ownership).
    //
    // The version number of a document will increase after each
    // change, including undo/redo. The number doesn't need to be
    // consecutive.
    property version: string read fVersion write fVersion;
  end;

  { TTextEdit }

  TTextEdit = class(TCollectionItem)
  private
    fRange: TRange;
    fNewText: string;
  private
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    // The range of the text document to be manipulated. To insert
    // text into a document create a range where start === end.
    property range: TRange read fRange write fRange;
    // The string to be inserted. For delete operations use an empty
    // string.
    property newText: string read fNewText write fNewText;
  end;

  TTextEdits = specialize TGenericCollection<TTextEdit>;

  { TTextDocumentEdit }

  TTextDocumentEdit = class(TPersistent)
  private
    fTextDocument: TVersionedTextDocumentIdentifier;
    fEdits: TTextEdits;
  published
    // The text document to change.
    property textDocument: TVersionedTextDocumentIdentifier read fTextDocument write fTextDocument;
    // The edits to be applied.
    property edits: TTextEdits read fEdits write fEdits;
  end;

  { TTextDocumentItem }

  TTextDocumentItem = class(TPersistent)
  private
    fUri: TDocumentUri;
    fLanguageId: string;
    fVersion: Integer;
    fText: string;
  published
    // The text document's URI.
    property uri: TDocumentUri read fUri write fUri;
    // The text document's language identifier.
    property languageId: string read fLanguageId write fLanguageId;
    // The version number of this document (it will increase after
    // each change, including undo/redo).
    property version: Integer read fVersion write fVersion;
    // The content of the opened text document.
    property text: string read fText write fText;
  end;

  { TTextDocumentPositionParams }

  TTextDocumentPositionParams = class(TPersistent)
  private
    fTextDocument: TTextDocumentIdentifier;
    fPosition: TPosition;
  published
    // The text document.
    property textDocument: TTextDocumentIdentifier read fTextDocument write fTextDocument;
    // The position inside the text document.
    property position: TPosition read fPosition write fPosition;
  end;
  
  { TRenameParams}
  TRenameParams=class(TTextDocumentPositionParams)
  private
    fNewName:String;
  published
    property newName: string read fNewName write fNewName;
  end;

  { TMarkupKind }

  { Describes the content type that a client supports in various
    result literals like `Hover`, `ParameterInfo` or
    `CompletionItem`.
  
    Please note that `MarkupKinds` must not start with a `$`. This
    kinds are reserved for internal usage. }
  
  TMarkupKind = record
  public const
    PlainText = 'plaintext';   // Plain text is supported as a content format
    Markdown = 'markdown';     // Markdown is supported as a content format
  private
    value: string;
  end;

  { TMarkupContent }

  { A `MarkupContent` literal represents a string value which content is interpreted base on its
    kind flag. Currently the protocol supports `plaintext` and `markdown` as markup kinds.
    
    If the kind is `markdown` then the value can contain fenced code blocks like in GitHub issues.
    See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting
       
    *Please Note* that clients might sanitize the return markdown. A client could decide to
    remove HTML from the markdown to avoid script execution. }

  TMarkupContent = class(TPersistent)
  private
    fKind: TMarkupKind;
    fValue: string;
  published
    // The type of the Markup
    property kind: string read fKind.value write fKind.value;
    // The content itself
    property value: string read fValue write fValue;
  public
    constructor Create(content: string; plainText: boolean = true);
  end;

  { TDiagnostic }


  TDiagnosticSeverity = ( __UNUSED__ = 0, 
                          Error = 1,
                          Warning = 2,
                          Information = 3,
                          Hint = 4
                          );

  TDiagnosticTag = ( // Unused or unnecessary code.
                     // Clients are allowed to render diagnostics with this tag faded out instead of having
                     // an error squiggle.
                     Unnecessary = 1,

                     //Deprecated or obsolete code.
                     //Clients are allowed to rendered diagnostics with this tag strike through.
                     Deprecated = 2
                     );
  TDiagnosticTags = set of TDiagnosticTag;

  // Represents a related message and source code location for a diagnostic. This should be
  // used to point to code locations that cause or are related to a diagnostics, e.g when duplicating
  // a symbol in a scope.
  TDiagnosticRelatedInformation = class (TCollectionItem)
  private
    fLocation: TLocation;
    fMessage: string;
  published
    // The location of this related diagnostic information.
    property location: TLocation read fLocation write fLocation;
    // The message of this related diagnostic information.
    property message: string read fMessage write fMessage;
  end;

  TDiagnosticRelatedInformationItems = specialize TGenericCollection<TDiagnosticRelatedInformation>;

  // Represents a diagnostic, such as a compiler error or warning. Diagnostic objects are only valid in the scope of a resource.

  TDiagnostic = class (TCollectionItem)
  private
    fRange: TRange;
    fSeverity: TDiagnosticSeverity;
    fCode: string;
    fSource: string;
    fMessage: string;
    fTags: TDiagnosticTags;
    fRelatedInformation: TDiagnosticRelatedInformationItems;
  published
    // The range at which the message applies.
    property range: TRange read fRange write fRange;

    // The diagnostic's severity. Can be omitted. If omitted it is up to the
    // client to interpret diagnostics as error, warning, info or hint.
    property severity: TDiagnosticSeverity read fSeverity write fSeverity;

    // The diagnostic's code, which might appear in the user interface.
    property code: string read fCode write fCode;

    // A human-readable string describing the source of this
    // diagnostic, e.g. 'typescript' or 'super lint'.
    property source: string read fSource write fSource;

    // The diagnostic's message.
    property message: string read fMessage write fMessage;

    // Additional metadata about the diagnostic.
    // @since 3.15.0
    // TODO: must be optional
    //property tags: TDiagnosticTags read fTags write fTags;

    // An array of related diagnostic information, e.g. when symbol-names within
    // a scope collide all definitions can be marked via this property. (OPTIONAL)
    // TODO: must be optional
    //property relatedInformation: TDiagnosticRelatedInformationItems read fRelatedInformation write fRelatedInformation;
  end;

  TDiagnosticItems = specialize TGenericCollection<TDiagnostic>;

  { TCommandKind}
  TCommandKind = record
  public const

    // Empty kind.
    Empty = '';

    // CompleteCode.
    CompleteCode = 'CompleteCode';

    //Get Unit Path
    GetUnitPath = 'GetUnitPath';

    // Remove Unused Units
    RemoveUnusedUnit='RemoveUnusedUnits';

  private
    value: string;
  end;

  { TCommand }

  TCommand = class(TPersistent)
  private
    fTitle: string;
    fCommand: string;
    fArguments: TStrings;
  published
    // Title of the command, like `save`.
    property title: string read fTitle write fTitle;
    // The identifier of the actual command handler.
    property command: string read fCommand write fCommand;
    // Arguments that the command handler should be  invoked with.
    property arguments: TStrings read fArguments write fArguments;
  public
    procedure AfterConstruction; override;
  end;

  { TWorkspaceEdit }

  { A workspace edit represents changes to many resources managed in the workspace. 
    The edit should either provide changes or documentChanges. 
    If the client can handle versioned document edits and if documentChanges are present, 
    the latter are preferred over changes. }

  { TChangeItems }

  TChangeItems=class(TCollection)
  private
    function GetItems(Index: integer): TTextEdit;
    procedure SetItems(Index: integer; AValue: TTextEdit);
  public
    constructor Create;
  public
    function Add: TTextEdit;
    property Items[Index: integer]: TTextEdit read GetItems write SetItems; default;
  end;
  
  TChangeDictory=specialize TFPGMap<TDocumentUri,TChangeItems>;

  TWorkspaceEdit = class (TPersistent)
  private
    fChanges:TChangeDictory;
  public
    constructor Create();
    destructor Destroy; override;

    function ToJson():TJSONData;
  published
    property changes:TChangeDictory read fChanges write fChanges;
  end;

  {
    NOTE(ryan): I couldn't figure out how to do this using the normal
    JSON RPC system and had to roll my own just to get it working for now
  }

  { TAbstractMessage }

  { A general message as defined by JSON-RPC. The language server 
    protocol always uses “2.0” as the jsonrpc version. }

  TAbstractMessage = class(TPersistent)
  private
    fJsonrpc: String;
    function GetJSONRPC: String;
  published
    property jsonrpc: String read GetJSONRPC;
  end;

  { TNotificationMessage }

  { A notification message. A processed notification message 
    must not send a response back. They work like events. }

  TNotificationMessage = class(TAbstractMessage)
  protected
    fMethod: string;
    fParams: TPersistent;
  published
    // The method to be invoked.
    property method: string read fMethod write fMethod;
    // The notification's params.
    // params?: Array<any> | object;
    property params: TPersistent read fParams write fParams;
  public
    procedure Send;
  end;

const
  ContentType = 'application/vscode-jsonrpc; charset=utf-8';

{ Utilities }

function PathToURI(path: String): TDocumentUri;
function UriToFilenameEx(uri:String):String;
implementation
uses
  lsp,URIParser;

{ Utilities }

function PathToURI(path: String): TDocumentUri;
begin
  Result:= FilenameToURI(path);
end;

function UriToFilenameEx(uri:String):String;
begin
    UriToFilename(uri,Result);
end;
{ TAbstractMessage }

function TAbstractMessage.GetJSONRPC: String;
begin
  result := '2.0';
end;

{ TNotificationMessage }

procedure TNotificationMessage.Send;
var
  Data: TJSONData;
  Content: String;
begin
  Data := specialize TLSPStreaming<TNotificationMessage>.ToJSON(self);
  if Data <> nil then
    begin
      Content := Data.AsJSON;

      WriteLn('Content-Type: ', ContentType);
      WriteLn('Content-Length: ', Content.Length);
      WriteLn;
      Write(Content);
      Flush(Output);

      Data.Free;
    end;
end;

{ TCommand }

procedure TCommand.AfterConstruction;
begin
  inherited;

  arguments := TStringList.Create;
end;

{ TPosition }

constructor TPosition.Create(l, c: integer);
begin
  line := l;
  character := c;
end;

{ TLocation }

constructor TLocation.Create(Path: String; Line, Column, EndLine,EndCol: Integer);
begin
  uri := PathToURI(Path);
  range := TRange.Create(Line, Column, EndLine,EndCol);
end;

{ TRange }

destructor TRange.Destroy;
begin
  if Assigned(fStart) then fStart.Free;
  if Assigned(fEnd) then fEnd.Free;
  inherited Destroy;
end;

constructor TRange.Create(line, column: integer);
begin
  fStart := TPosition.Create(line, column);
  fEnd := TPosition.Create(line, column);
end;

constructor TRange.Create(line, column, len: integer);
begin
  fStart := TPosition.Create(line, column);
  fEnd := TPosition.Create(line, column + len);
end;

constructor TRange.Create(startLine, startColumn: integer; endLine, endColumn: integer); overload;
begin
  fStart := TPosition.Create(startLine, startColumn);
  fEnd := TPosition.Create(endLine, endColumn);
end;

{ TMarkupContent }

constructor TMarkupContent.Create(content: string; plainText: boolean = true);
begin
  value := content;
  if plainText then
    kind := TMarkupKind.PlainText
  else
    kind := TMarkupKind.Markdown;
end;

{ TOptional }

function TOptional.GetValue: T;
begin
  if fHasValue then Result := fValue
  else Exception.Create('no value');
end;

procedure TOptional.SetValue(AValue: T);
begin
  fValue := AValue;
  fHasValue := True;
end;

procedure TOptional.Clear;
begin
  fHasValue := False;
end;

{ TOptionalVariant }

function TOptionalVariant.GetValue: T;
begin
  Result := T(inherited Value);
end;

procedure TOptionalVariant.SetValue(AValue: T);
begin
  inherited Value := AValue;
end;

constructor TOptionalVariant.Create;
begin
  inherited Create;
end;

constructor TOptionalVariant.Create(AValue: T);
begin
  Create;
  SetValue(AValue);
end;

{ TOptionalObject }

function TOptionalObject.GetValue: T;
begin
  Result := T(inherited Value);
end;

procedure TOptionalObject.SetValue(AValue: T);
begin
  inherited Value := AValue;
end;

constructor TOptionalObject.Create;
begin
  inherited Create;
end;

constructor TOptionalObject.Create(AValue: T);
begin
  Create;
  SetValue(AValue);
end;

function TOptionalObject.ValueClass: TClass;
begin
  Result := T;
end;

{ TGenericCollection }

constructor TGenericCollection.Create;
begin
  inherited Create(T);
end;

constructor TWorkspaceEdit.Create;
begin
  self.changes:=TChangeDictory.Create;
  inherited Create;
end;

destructor TWorkspaceEdit.Destroy;
var i:Integer;
begin
  if Assigned(self.changes) then
  begin
    for i := 0 to self.changes.Count-1 do
    begin
      self.changes.Data[i].Free;
    end;
    self.changes.Free;
  end;
  inherited Destroy;
end;

function TWorkspaceEdit.ToJson: TJSONData;
var json:TJSONObject;
  data:TJSONData;
  key:TDocumentUri;
  val:TTextEdit;
  Items:TChangeItems;
  i:Integer;
begin

  json:=TJSONObject.Create;

   for i := 0 to self.changes.Count-1 do
   begin
     key:=self.changes.Keys[i];
     Items:=self.changes.Data[i];
     data:=specialize TLSPStreaming<TChangeItems>.ToJSON(Items);

     json.Add(key,data);
   end;
   Result:=TJSONObject.Create;
   TJSONObject(Result).Add('changes',json);
  
 
end;

function TChangeItems.GetItems(Index: integer): TTextEdit;
begin
  Result := TTextEdit(inherited Items[Index]);
end;

procedure TChangeItems.SetItems(Index: integer; AValue: TTextEdit);
begin
  Items[Index].Assign(AValue);
end;

constructor TChangeItems.Create;
begin
  inherited Create(TTextEdit);
end;

function TChangeItems.Add: TTextEdit;
begin
  Result := inherited Add as TTextEdit;
end;

constructor TTextEdit.Create(ACollection: TCollection);
begin
  self.fRange:=TRange.Create(0,0);
  inherited Create(ACollection);
end;

destructor TTextEdit.Destroy;
begin
  if Assigned(self.range) then
  begin
    FreeAndNil(self.fRange);
  end;
  inherited Destroy;
end;

end.
