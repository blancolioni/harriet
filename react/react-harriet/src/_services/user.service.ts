import $ from 'jquery';

export const userService = {
    login,
    postRequest,
    getRequest
};

const serverName = 'localhost';
const port = '8080';

const serverUrl = 'http://' + serverName + ':' + port + '/';

export interface LoginData {
    id: string
    token: string
    faction: string
}

async function login(username : string, password : string) : Promise<LoginData> {
    const requestOptions = {
        method: 'POST',
    };

    const response = await fetch(serverUrl + 'login?user=' + username + '&password=' + password, requestOptions);
    const data = await handleResponse(response);
    localStorage.setItem('token', data.id);
    return data;
}

async function handleResponse(response : Response) : Promise<LoginData> {
    const text = await response.text();
    const data = text && JSON.parse(text);
    if (!response.ok) {
        if (response.status === 401) {
            return Promise.reject('authorization failed');
        }
        const error = (data && data.message) || response.statusText;
        console.log('handleResponse', error);
        return Promise.reject(error);
    }
    return data;
}

function sendRequest(serviceName : string, requestArgs : any, fetchArgs : any) : Promise<any> {
    requestArgs = requestArgs || {};
    requestArgs.id = localStorage.getItem('token');
    fetchArgs = fetchArgs || '';

    return fetch(serverUrl + serviceName + '?' + $.param(requestArgs), fetchArgs)
    .then((response) => response.json());
}

function postRequest(serviceName : string, args : any = {}) : Promise<any> {
    return sendRequest(serviceName, args, { method: "POST" });
}

function getRequest(serviceName : string, args : any = {}) : Promise<any> {
    return sendRequest(serviceName, args, { method: "GET" });
}

