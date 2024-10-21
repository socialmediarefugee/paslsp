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

unit window;

{$mode objfpc}{$H+}

interface

uses
  Classes, 
  CodeToolManager, CodeCache,
  lsp, basic;

// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#window_showMessage


type
  
  { TMessageType }

  TMessageType = (
    __UNUSED__ = 0,
    // An error message.
    Error = 1,
    // A warning message.
    Warning = 2,
    // An information message.
    Info = 3,
    // A log message.
    Log = 4
  );

  { TShowMessageParams }

  TShowMessageParams = class(TPersistent)
  private
    fType: TMessageType;
    fMessage: string;
    fHasFile: Boolean;
  published
    // The message type.
    property &type: TMessageType read fType write fType;
    // The actual message.
    property message: string read fMessage write fMessage;
    // The Unit File
    property hasFile: Boolean read fHasFile write fHasFile;
  end;

  { TShowMessageNotification }
  { The show message notification is sent from a server to a client to ask 
    the client to display a particular message in the user interface. }

  TShowMessageNotification = class(TNotificationMessage)
  public
    constructor Create(_type: TMessageType; Message: String;_hasfile:Boolean=False);
    destructor Destroy; override;
  end;

  TMessageActionItem = class(TCollectionItem)
  private
    fTitle: string;
  published
    // A short title like 'Retry', 'Open Log' etc.
    property title: string read fTitle write fTitle;
  end;

  TMessageActionItems = specialize TGenericCollection<TMessageActionItem>;

  { TShowMessageRequstParams }

  TShowMessageRequstParams = class(TShowMessageParams)
  private
    fActions: TMessageActionItems;
  published
    // The message action items to present.
    property actions: TMessageActionItems read fActions write fActions;
  end;


  { The show message request is sent from a server to a client to ask the 
    client to display a particular message in the user interface. In addition 
    to the show message notification the request allows to pass actions 
    and to wait for an answer from the client. }

  //TShowMessageRequest = class(specialize TLSPRequest<TShowMessageRequstParams, TMessageActionItem>)
  //  function Process(var Params: TShowMessageRequstParams): TMessageActionItem; override;
  //end;

implementation

{ TShowMessageNotification }

constructor TShowMessageNotification.Create(_type: TMessageType; Message: String;_hasfile:Boolean);
begin
  params := TShowMessageParams.Create;
  TShowMessageParams(params).&type := _type;
  TShowMessageParams(params).message := Message;
  TShowMessageParams(params).hasFile := _hasfile;
  method := 'window/showMessage';
end;

destructor TShowMessageNotification.Destroy;
begin
  params.Free;
  inherited;
end;

{ TShowMessageRequest }

//function TShowMessageRequest.Process(var Params: TShowMessageRequstParams): TMessageActionItem;
//begin
//  result := nil
//end;

end.
