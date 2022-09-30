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

unit inactiveRegions;

{$mode objfpc}{$H+}

interface

uses
  Classes, 
  lsp, basic;

// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#window_showMessage
type
  { TInputRegion }
  TInputRegion=class(TCollectionItem)
  private
    fStartline:Integer;
    fStartCol:Integer;
    fEndline:Integer;
    fEndCol:Integer;
  published
    property startLine: Integer read fStartline write fStartline;
    property startCol: Integer read fStartCol write fStartCol;
    property endLine: Integer read fEndline write fEndline;
    property endCol: Integer read fEndCol write fEndCol;
  end;
   
  TRegionsItems = specialize TGenericCollection<TInputRegion>;

  { TInactiveRegionParams }
  TInactiveRegionParams=class(TPersistent)
  private
    fUri:string;
    fFileVersion:Integer;
    fRegions:TRegionsItems; 
  published
    property uri: string read fUri write fUri;
    property fileVersion:Integer read fFileVersion write fFileVersion;
    property regions:TRegionsItems read fRegions write fRegions;
  end;


  { TInactiveRegionsNotification }
  { The  message notification is sent from a server to a client to ask 
    the client to display a inactive region in the user interface. }

  TInactiveRegionsNotification = class(TNotificationMessage)
  public
    constructor Create(uri:string; regions:TRegionsItems);
    destructor Destroy; override;
  end;

implementation

{ TShowMessageNotification }

constructor TInactiveRegionsNotification.Create(uri:string; regions:TRegionsItems);
begin
  params := TInactiveRegionParams.Create;
  TInactiveRegionParams(params).regions:=regions;
  TInactiveRegionParams(params).uri:=uri;
  TInactiveRegionParams(params).fileVersion:=1;
  method := 'pasls/inactiveRegions';
end;

destructor TInactiveRegionsNotification.Destroy;
begin
  params.Free;
  inherited;
end;

end.
