// Copyright (C) 2014, 2015, 2016 Jan Moringen
//
// Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

/// Generic utilities

function getURLParameter(name) {
    var regex = new RegExp('[?|&]' + name + '=' + '([^&;]+?)(&|#|;|$)');
    return decodeURIComponent((regex.exec(location.search)
                               || [,""])
                              [1].replace(/\+/g, '%20'))
        || null;
}

function showMessage(message) {
    $('#messages')
        .removeClass('hidden')
        .append('<div class="alert alert-danger" role="alert">'
                + message
                + '<br/>'
                + '<a href="root.html">Back to introspection start page.</a>'
                + '</div>');
}

/// Introspection data

function findHost(data, id) {
    var result;
    $.each(data.children, function(i, host) {
        if (host.id === id) {
            result = host;
            return false;
        }
    });
    return result;
}

function findProcess(host, id) {
    var result;
    $.each(host.children, function(i, process) {
        if (process['process-id'] == id) {
            result = process;
            return false;
        }
    });
    return result;
}

function findParticipant(data, id) {
    var result;
    $.each(data.children, function(i, host) {
        $.each(host.children, function(i, process) {
            function rec(i, participant) {
                if (participant.id === id) {
                    result = { 'host':        host,
                               'process':     process,
                               'participant': participant };
                    return false;
                }
                $.each(participant.children, rec);
                if (result) return false;
            }
            $.each(process.children, rec);
            if (result) return false;
        })
    });
    return result;
}

// Search

function makeSearchURL(kind, spec) {
    var result = 'search.html?query=';
    switch (kind) {
    case "host":
        result += 'children/host';
        break;
    case "process":
        result += 'children/host/children/process';
        break;
    default:
        result += './/' + kind;
    }

    var keys = Object.keys(spec);
    if (keys.length  == 1 && spec[keys[0]] === undefined) {
        result += '/@' + keys[0];
    } else {
        result += '[';
        var first  = true;
        $.each(spec, function (k, v) {
            if (!first) {
                result += ' and ';
            }
            result += '@' + k + '=&quot;' + encodeURIComponent(v) + '&quot;';
            first = false;
        });
        result += ']';
    }
    return result;
}

// Breadcrumbs

function hostDescription(host) {
    return 'Host ' + host.hostname;
}
function hostURL(host, level) {
    if (level === 'host')
        return 'javascript:refresh("' + host.id + '")';
    else
        return 'host.html?hostId=' + host.id;
}

function processDescription(process) {
    return 'Process '
        + (process['display-name'] || process['program-name'])
        + '[' + process['process-id'] + ']';
}
function processURL (host, process, level) {
    if (level === 'process')
        return 'javascript:refresh("' + process['process-id'] + '")';
    else
        return 'process.html?hostId=' + host.id + '&processId=' + process['process-id'];
}

function participantDescription (participant) {
    return participant.kind + ' ' + participant.id.substr(0, 8);
}
function participantURL(host, process, participant, level) {
    if (level === 'participant')
        return 'javascript:refresh("' + participant.id + '")';
    else
        return ('participant.html?hostId=' + host.id
                + '&processId=' + process['process-id']
                + '&participantId=' + participant.id);
}

//

function updatePlot(plot, data) {
    var series = $.map(data, function (value, index) { return [ [ index, value ] ]; });
    plot.setData([ series ]);
    plot.setupGrid();
    plot.draw();
}

function updatePlotAndValue(plot, labelElement, data) {
    updatePlot(plot, data['history']);
    if (data.value) {
        labelElement.html(data.value.toFixed(3));
    }
}
