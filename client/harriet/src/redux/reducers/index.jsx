import { combineReducers } from "redux";

import login from "./login";
import boxes from "./boxes";
import status from "./status";
import faction from "./faction";

export default combineReducers({ login, boxes, status, faction });
