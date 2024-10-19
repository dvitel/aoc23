#!/bin/bash
docker run -i -t --rm -v ./data:/data -v ./uiua:/app --name uiua uiua uiua watch --clear