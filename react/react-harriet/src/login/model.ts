export type State = {
    loggedIn      : boolean;
    submitted     : boolean;
    loading       : boolean;
    token?        : string;
    userName?     : string;
    factionName?  : string;
    error?        : string;
    errorMessage? : string;
};
