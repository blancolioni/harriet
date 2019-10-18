import { combineReducers } from "redux";

import login from "../login/reducer";
import boxes from "./boxes";
import status from "./status";
import faction from "./faction";

export default combineReducers({ login, boxes, status, faction });
