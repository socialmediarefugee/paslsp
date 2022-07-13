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

unit workspace;

{$mode objfpc}{$H+}

interface

uses
  Classes, fpjson, fpjsonrpc,
  lsp, basic, general, documentSymbol, settings;

type
  
  { TWorkspaceFoldersChangeEvent }

  TWorkspaceFoldersChangeEvent = class(TPersistent)
  private
    fAdded: TWorkspaceFolderItems;
    fRemoved: TWorkspaceFolderItems;
  published
    // The array of added workspace folders
    property added: TWorkspaceFolderItems read fAdded write fAdded;
    // The array of the removed workspace folders
    property removed: TWorkspaceFolderItems read fRemoved write fRemoved;
  end;

  { TDidChangeWorkspaceFoldersParams }

  TDidChangeWorkspaceFoldersParams = class(TPersistent)
  private
    fEvent: TWorkspaceFoldersChangeEvent;
  published
    property event: TWorkspaceFoldersChangeEvent read fEvent write fEvent;
  end;

  { TDidChangeWorkspaceFolders }

  { The workspace/didChangeWorkspaceFolders notification is sent from the client to the server 
    to inform the server about workspace folder configuration changes. The notification is sent 
    by default if both client capability workspace.workspaceFolders and the server capability 
    workspace.workspaceFolders.supported are true; or if the server has registered itself to 
    receive this notification. To register for the workspace/didChangeWorkspaceFolders send 
    a client/registerCapability request from the server to the client. The registration parameter 
    must have a registrations item of the following form, where id is a unique id used to 
    unregister the capability (the example uses a UUID): } 

  TDidChangeWorkspaceFolders = class(specialize TLSPNotification<TDidChangeWorkspaceFoldersParams>)
    procedure Process(var Params : TDidChangeWorkspaceFoldersParams); override;
  end;

  { The parameters of a Workspace Symbol Request. }

  TWorkspaceSymbolParams = class(TPersistent)
  private
    fQuery: string;
  published
    // A query string to filter symbols by. Clients may send an empty
    // string here to request all symbols.
    property query: string read fQuery write fQuery;
  end;

  { TWorkspaceSymbolRequest }

  { The workspace symbol request is sent from the client to the server to 
    list project-wide symbols matching the query string. }

  TWorkspaceSymbolRequest = class(specialize TLSPRequest<TWorkspaceSymbolParams, TSymbolInformationItems>)
    function DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData; override;
  end;

  { TDidChangeConfigurationParams }

  TDidChangeConfigurationParams = class(TPersistent)
  private
    fSettings: TServerSettings;
  published
    property settings: TServerSettings read fSettings write fSettings;
  end;

  { TDidChangeConfiguration }

  { A notification sent from the client to the server to signal the change of configuration settings. }

  TDidChangeConfiguration = class(specialize TLSPNotification<TDidChangeConfigurationParams>)
    procedure Process(var Params: TDidChangeConfigurationParams); override;
  end;

  { TExecuteCommandParams }

  TExecuteCommandParams  = class(TPersistent)
  private
    fCommand: string;
    fArguments: TStrings;
  published
    // The identifier of the actual command handler.
    property command: string read fCommand write fCommand;
    // Arguments that the command handler should be  invoked with.
    property arguments: TStrings read fArguments write fArguments;
  public
    procedure AfterConstruction; override;
  end;


  TExecuteCommand = class(specialize TLSPRequest<TExecuteCommandParams, TPersistent>)

     function Process(var Params : TExecuteCommandParams): TPersistent; override;
  private
     procedure DoCommandComplateCode(params:TExecuteCommandParams);
  end;

implementation
uses
  SysUtils, DateUtils,CodeToolManager,URIParser,CodeCache,SetSelection,window;

{ TExecuteCommand }

function TExecuteCommand.Process(var Params: TExecuteCommandParams): TPersistent;
begin
  if Params.command=TCommandKind.CompleteCode then
     self.DoCommandComplateCode(Params);
  Result:=nil;
end;

procedure TExecuteCommand.DoCommandComplateCode(params: TExecuteCommandParams);
var uri:String;
  fileName:String;
  x,y,TopLine:Integer;
  code,newCode:TCodeBuffer;
  newx,newy,newTopLine,BlockTopLine, BlockBottomLine:Integer;
  Notify:TShowMessageNotification;
  pos:TPosition;
begin
  uri:=params.arguments.Strings[0];
  y:=StrToInt(params.arguments.Strings[1]);
  x:=StrToInt(params.arguments.Strings[2]);
  URIToFilename(uri,fileName);
  Code := CodeToolBoss.FindFile(fileName);
  if Code=nil then
  Code:=CodeToolBoss.LoadFile(fileName,true,false);

  if Code=nil then exit;

  code.Clear;
  if not code.Reload then exit;

  TopLine:=0;
  if CodeToolBoss.CompleteCode(code,x+1,y+1,TopLine,newCode,newx,newy,newTopLine,BlockTopLine,BlockBottomLine,True) then
  begin
    if Assigned(newCode) then
    begin
      newCode.Save;
      Notify:=TShowMessageNotification.Create(TMessageType.Info,'Complete codes succeed!',true);
      notify.Send;
      notify.Free;
      //document not loaded now ,so it's not   correct
      //pos:=TPosition.Create;
      //pos.line:=newy+1;
      //pos.character:=newx+1;
      //Notify:=TSetSelectionNotification.Create(uri,pos,pos);
      //Notify.Send;
      //Notify.Free;
      //pos.Free;

    end;

  end;
end;

{ TDidChangeConfiguration }

procedure TDidChangeConfiguration.Process(
  var Params: TDidChangeConfigurationParams);
begin

end;


{ TExecuteCommandParams }

procedure TExecuteCommandParams.AfterConstruction;
begin
  arguments := TStringList.Create;
end;



{ TDidChangeWorkspaceFolders }

procedure TDidChangeWorkspaceFolders.Process(var Params : TDidChangeWorkspaceFoldersParams);
begin with Params do
  begin
    // TODO: ST3 is not sending this message reliably but it will be ready in ST4
  end;
end;

{ TWorkspaceSymbolRequest }

function TWorkspaceSymbolRequest.DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData;
var
  Input: TWorkspaceSymbolParams;
  StartTime: TDateTime;
begin
  Input := specialize TLSPStreaming<TWorkspaceSymbolParams>.ToObject(Params);
  StartTime := Now;
  Result := SymbolManager.FindWorkspaceSymbols(Input.query);
  //writeln(stderr,'workspace/symbol payload=', ConvertBytesToHumanReadable(Length(Result.AsJson)), ' in ', MilliSecondsBetween(Now,StartTime), 'ms');
  //writeln(stderr, Result.AsJson);
  Flush(stderr);
  Input.Free;

  if not Assigned(Result) then
    Result := TJSONNull.Create;
end;

initialization
  LSPHandlerManager.RegisterHandler('workspace/didChangeConfiguration', TDidChangeConfiguration);
  LSPHandlerManager.RegisterHandler('workspace/didChangeWorkspaceFolders', TDidChangeWorkspaceFolders);
  LSPHandlerManager.RegisterHandler('workspace/symbol', TWorkspaceSymbolRequest);
  LSPHandlerManager.RegisterHandler('workspace/executeCommand', TExecuteCommand);
end.
