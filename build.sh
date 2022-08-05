#!/bin/bash
SCRIPT=$(readlink -f "$0")
SCRIPTPATH=$(dirname "$SCRIPT")

dotnet tool restore
dotnet paket restore
dotnet adaptify --local $SCRIPTPATH/src/Aardwars/aardwars.fsproj
dotnet build -c Release Aardwars.NonWindows.sln