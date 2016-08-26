// Copyright (C) 2014, 2015, 2016 Jan Moringen
//
// Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

var defaultIntrospectionBaseURL = '/api/introspection';

function Introspection (baseURL) {
    this.baseURL = baseURL || defaultIntrospectionBaseURL;
}

/*
 * Wrap the done and fail functions of the future to appropriately
 * unpack success and failure results.
 */
function augmentFuture(future) {
    var fail;
    return {
        done: function (callback) {
            future.done(function (result) {
                if (typeof result['error'] === 'undefined') {
                    return callback(result.data);
                } else {
                    return fail(result.error);
                }
            });
            return this;
        },
        fail: function (callback) {
            fail = callback;
            future.fail(function (result) {
                var message = result.responseJSON
                    ? result.responseJSON.error
                    : result.statusText;
                callback(message);
            });
            return this;
        }
    };
}

/**
 * Retrieves a snapshot of the introspection state from the server.
 *
 * @return {Object} A deferred object yielding the snapshot.
 */
Introspection.prototype.get = function () {
    return augmentFuture($.getJSON(this.baseURL + '/snapshot'));
};

/**
 * Queries the introspection database of the server.
 *
 * @param {Object} query An object containing query parameters.
 *
 *   Must contain a "query" property holding the query string.
 *
 *   May contain a "start" property specifying the first element in
 *   the result sequence that should be return. Prior elements are
 *   skipped.
 *
 *   May contain a "limit" property controlling the maximum number of
 *   returned elements.
 *
 * @return {Object} A deferred object yielding an array of search
 *   results.
 */
Introspection.prototype.search = function (query) {
    return augmentFuture($.getJSON(this.baseURL + '/search', query));
};
