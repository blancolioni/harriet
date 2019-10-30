import { ClientState } from '../clients/model';

interface TableCell {
  value : any
  display : string
}

interface TableRow {
  [key : string] : TableCell
}

interface ColumnHeading {
  id : string
  label : string
}
export interface State extends ClientState {
  tableData : TableRow[]
  headings : ColumnHeading[]
  sortColumn : number
  sortAscending : boolean
}

export const tableInitialState = (baseState : ClientState) => ({
  ...baseState,
  tableData: [],
  headings: [],
  sortColumn: -1,
  sortAscending: true,
});
