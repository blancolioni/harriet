import { combineReducers } from "redux";

import login from "./login";
import boxes from "./boxes";
import status from "./status";

export default combineReducers({ login, boxes, status });
