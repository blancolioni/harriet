export const userService = {
    login,
};

const serverName = 'localhost';
const port = '8080';

const serverUrl = 'http://' + serverName + ':' + port + '/';

async function login(username : string, password : string) : Promise<string> {
    const requestOptions = {
        method: 'POST',
    };

    const response = await fetch(serverUrl + 'login?user=' + username + '&password=' + password, requestOptions);
    const data = await handleResponse(response);
    return data;
}

async function handleResponse(response : Response) : Promise<string> {
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
    return data.id;
}