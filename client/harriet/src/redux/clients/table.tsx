import { ModelState } from './client';

interface TableCell {
    value : string
}

interface TableRow {
    [key : string] : TableCell
}

interface ColumnHeading {
    id : string
    label : string
}
export interface TableState extends ModelState {
    tableData : TableRow[]
    headings : ColumnHeading[]
    sortColumn : number
    sortAscending : boolean
}

export const initialTableState : TableState = {
    title: 'Table',
    tableData: [],
    headings: [],
    sortColumn: -1,
    sortAscending: true,
}

